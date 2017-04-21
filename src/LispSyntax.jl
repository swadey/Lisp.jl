__precompile__()

module LispSyntax
include("parser.jl")
export sx, desx, codegen, @lisp, @lisp_str, assign_reader_dispatch

# Internal types
type s_expr
  vector
end

sx(x...) = s_expr([x...])
==(a :: s_expr, b :: s_expr) = a.vector == b.vector


function desx(s)
  if typeof(s) == s_expr
    return map(desx, s.vector)
  elseif isa(s, Dict)
    return Dict(map(x -> desx(x[1]) => desx(x[2]), s)...)
  elseif isa(s, Set)
    return Set(map(desx, s))
  else
    return s
  end
end

function lispify(s)
  if isa(s, s_expr)
    return "(" * join(map(lispify, s.vector), " ") * ")"
  else
    return "$s"
  end
end

function construct_sexpr(items...) # convert the input tuple to an array
  ret = Array(Any, length(items))
  for i = 1:length(items)
    ret[i] = items[i]
  end
  ret
end

function assign_reader_dispatch(sym, fn)
  reader_table[sym] = fn
end

function quasiquote(s, escape_exceptions)
  if isa(s, Array) && length(s) == 2 && s[1] == :splice
    codegen(s[2], escape_exceptions = escape_exceptions)
  elseif isa(s, Array) && length(s) == 2 && s[1] == :splice_seq
    Expr(:..., codegen(s[2], escape_exceptions = escape_exceptions))
  elseif isa(s, Array)
    Expr(:call, :construct_sexpr, map(s -> quasiquote(s, escape_exceptions), s)...)
  elseif isa(s, Symbol)
    Expr(:quote, s)
  else
    s
  end
end

function quote_it(s)
  if isa(s, Array)
    Expr(:call, :construct_sexpr, map(s -> quote_it(s), s)...)
  elseif isa(s, Symbol)
   QuoteNode(s)
  else
    s
  end
end

function codegen(s; escape_exceptions = Set{Symbol}())
  if isa(s, Symbol)
    if s in escape_exceptions
      s
    else
      esc(s)
    end
  elseif isa(s, Dict)
    coded_s = map(x -> Expr(symbol("=>"),
                            codegen(x[1], escape_exceptions = escape_exceptions),
                            codegen(x[2], escape_exceptions = escape_exceptions)), s)
    Expr(:call, :Dict, coded_s...)
  elseif isa(s, Set)
    coded_s = map(x -> codegen(x, escape_exceptions = escape_exceptions), s)
    Expr(:call, :Set, Expr(:vect, coded_s...))
  elseif !isa(s, Array) # constant
    s
  elseif length(s) == 0 # empty array
    s
  elseif s[1] == :if
    if length(s) == 3
      :($(codegen(s[2], escape_exceptions = escape_exceptions)) && $(codegen(s[3], escape_exceptions = escape_exceptions)))
    elseif length(s) == 4
      :($(codegen(s[2], escape_exceptions = escape_exceptions)) ? $(codegen(s[3], escape_exceptions = escape_exceptions)) : $(codegen(s[4],  escape_exceptions = escape_exceptions)))
    else
      throw("illegal if statement $s")
    end
  elseif s[1] == :def
    assert(length(s) == 3)
    :($(esc(s[2])) = $(codegen(s[3], escape_exceptions = escape_exceptions)))
  elseif s[1] == :let
    syms     = Set([ s[2][i] for i = 1:2:length(s[2]) ])
    bindings = [ :($(s[2][i]) = $(codegen(s[2][i+1], escape_exceptions = escape_exceptions ∪ syms))) for i = 1:2:length(s[2]) ]
    coded_s  = map(x -> codegen(x, escape_exceptions = escape_exceptions ∪ syms), s[3:end])
    Expr(:let, Expr(:block, coded_s...), bindings...)
  elseif s[1] == :while
    coded_s = map(x -> codegen(x, escape_exceptions = escape_exceptions), s[2:end])
    Expr(:while, coded_s[1], Expr(:block, coded_s[2:end]...))
  elseif s[1] == :for
    syms     = Set([ s[2][i] for i = 1:2:length(s[2]) ])
    bindings = [ :($(s[2][i]) = $(codegen(s[2][i+1], escape_exceptions = escape_exceptions ∪ syms))) for i = 1:2:length(s[2]) ]
    coded_s  = map(x -> codegen(x, escape_exceptions = escape_exceptions ∪ syms), s[3:end])
    Expr(:for, Expr(:block, bindings...), Expr(:block, coded_s...))
  elseif s[1] == :do
    Expr(:block, map(x -> codegen(x, escape_exceptions = escape_exceptions), s[2:end])...)
  elseif s[1] == :global
    Expr(:global, map(x -> esc(x), s[2:end])...)
  elseif s[1] == :quote
    quote_it(s[2])
  elseif s[1] == :import
     Expr(:using, map(x -> esc(x), s[2:end])...)
  elseif s[1] == :splice
    throw("missplaced ~ (splice)")
  elseif s[1] == :splice_seq
    throw("missplaced ~@ (splice_seq)")
  elseif s[1] == :quasi
    quasiquote(s[2], escape_exceptions)
  elseif s[1] == :lambda || s[1] == :fn
    assert(length(s) >= 3)
    coded_s = map(x -> codegen(x, escape_exceptions = escape_exceptions ∪ Set(s[2])), s[3:end])
    Expr(:function, Expr(:tuple, s[2]...), Expr(:block, coded_s...))
  elseif s[1] == :defn
    # Note: julia's lambdas are not optimized yet, so we don't define defn as a macro.
    #       this should be revisited later.
    coded_s = map(x -> codegen(x, escape_exceptions = escape_exceptions ∪ Set(s[3])), s[4:end])
    Expr(:function, Expr(:call, esc(s[2]), s[3]...), Expr(:block, coded_s...))
  elseif s[1] == :defmacro
     Expr(:macro, Expr(:call, esc(s[2]), s[3]...),
          begin
            coded_s = map(x -> codegen(x, escape_exceptions = escape_exceptions ∪ Set(s[3])), s[4:end])
            sexpr = Expr(:block, coded_s...) #codegen(s[4], escape_exceptions = escape_exceptions ∪ Set(s[3]))
            :(codegen($sexpr, escape_exceptions = $escape_exceptions ∪ Set($(s[3]))))
          end)
  elseif s[1] == :defmethod
    # TODO
  else
    coded_s = map(x -> codegen(x, escape_exceptions = escape_exceptions), s)
    if (typeof(coded_s[1]) == Symbol && ismatch(r"^@.*$", string(coded_s[1]))) ||
       (typeof(coded_s[1]) == Expr && ismatch(r"^@.*$", string(coded_s[1].args[1])))
      Expr(:macrocall, coded_s[1], coded_s[2:end]...)
    else
      Expr(:call, coded_s[1], coded_s[2:end]...)
    end
  end
end

"This is an internal helper function, do not call outside of package"
function lisp_eval_helper(str :: AbstractString)
  s = desx(LispSyntax.read(str))
  return codegen(s)
end
    
macro lisp(str)
  return lisp_eval_helper(str)
end

macro lisp_str(str)
  return lisp_eval_helper(str)
end

#==
Some of following code is derived from Cxx.jl.
> Copyright (c) 2013-2016: Keno Fischer and other contributors
> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
> The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
==#
import Base: LineEdit, REPL

function process_line(line)
  try
    eval(Main, Expr(:macrocall, Symbol("@lisp"), line))
  catch e
    print_with_color(:red, STDERR, "ERROR: ", sprint(showerror, e), "\n")
  end
end

function create_lisp_REPL(prompt, name, repl=Base.active_repl, main_mode=repl.interface.modes[1])
  mirepl = isdefined(repl,:mi) ? repl.mi : repl

  panel = LineEdit.Prompt(prompt;
    on_enter = s -> try
      lisp_eval_helper(String(LineEdit.buffer(s).data))
      true
    catch e
      isa(e, ParserCombinator.ParserException) ? false : rethrow(e)
    end)

  panel.on_done = REPL.respond(process_line,repl,panel)

  main_mode == mirepl.interface.modes[1] &&
    push!(mirepl.interface.modes,panel)

  hp = main_mode.hist
  hp.mode_mapping[name] = panel
  panel.hist = hp

  search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
  mk = REPL.mode_keymap(main_mode)

  b = Dict{Any,Any}[skeymap, mk, LineEdit.history_keymap, LineEdit.default_keymap, LineEdit.escape_defaults]
  panel.keymap_dict = LineEdit.keymap(b)
  
  panel
end

function run_lisp_REPL(prompt="lisp> ", name=:lisp, key='<')
  repl = Base.active_repl
  mirepl = isdefined(repl,:mi) ? repl.mi : repl
  main_mode = mirepl.interface.modes[1]

  panel = create_lisp_REPL(prompt, name)

  # Install this mode into the main mode
  const lisp_keymap = Dict{Any,Any}(
    key => function (s,args...)
      if isempty(s) || position(LineEdit.buffer(s)) == 0
        buf = copy(LineEdit.buffer(s))
        LineEdit.transition(s, panel) do
          LineEdit.state(s, panel).input_buffer = buf
        end
      else
        LineEdit.edit_insert(s,key)
      end
    end
  )
  main_mode.keymap_dict = LineEdit.keymap_merge(main_mode.keymap_dict, lisp_keymap);
  nothing
end

if isdefined(Base, :active_repl)
  run_lisp_REPL()
end

end # module
