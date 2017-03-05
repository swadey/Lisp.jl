__precompile__()

module LispSyntax

import Base: ==, length

export sx, desx, codegen, @lisp, @lisp_str, assign_reader_dispatch

include("parser.jl")

# Internal types
type SExpr
  vector::Vector{Any}
end

sx(x...) = SExpr([x...])

==(a::SExpr, b::SExpr) = a.vector == b.vector

length(x::SExpr) = length(x.vector)

desx(s) = s
desx(s::SExpr) = map(desx, s.vector)
desx(s::Dict) = Dict(map(x -> desx(x[1]) => desx(x[2]), s)...)
desx(s::Set) = Set(map(desx, s))

lispify(s) = string(s)
lispify(s::SExpr) = string("(", join(map(lispify, s.vector), " "), ")")

"Convert the input tuple to an array."
construct_sexpr(items...) = collect(items)

function assign_reader_dispatch(sym, fn)
  reader_table[sym] = fn
end

quasiquote(s, escape_exceptions::Set{Symbol}) = s
quasiquote(s::Symbol, escape_exceptions::Set{Symbol}) = Expr(:quote, s)

function quasiquote(s::Vector{Any}, escape_exceptions::Set{Symbol})
    len_s = length(s)
    if len_s == 2 && s[1] == :splice
        codegen(s[2], escape_exceptions = escape_exceptions)
    elseif len_s == 2 && s[1] == :splice_seq
        Expr(:..., codegen(s[2], escape_exceptions = escape_exceptions))
    else
        Expr(:call, :construct_sexpr, map(s -> quasiquote(s, escape_exceptions), s)...)
    end
end

quote_it(s) = s
quote_it(s::Symbol) = QuoteNode(s)
quote_it(s::Vector{Any}) = Expr(:call, :construct_sexpr, map(s -> quote_it(s), s)...)

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
  elseif isempty(s) # empty array
    s
  elseif s[1] == :if
    len_s = length(s)
    if len_s == 3
      :($(codegen(s[2], escape_exceptions = escape_exceptions)) && $(codegen(s[3], escape_exceptions = escape_exceptions)))
    elseif len_s == 4
      :($(codegen(s[2], escape_exceptions = escape_exceptions)) ? $(codegen(s[3], escape_exceptions = escape_exceptions)) : $(codegen(s[4],  escape_exceptions = escape_exceptions)))
    else
      error("illegal if statement $s")
    end
  elseif s[1] == :def
    assert(length(s) == 3)
    :($(esc(s[2])) = $(codegen(s[3], escape_exceptions = escape_exceptions)))
  elseif s[1] == :let
    len_s2 = length(s[2])
    syms     = Set{Symbol}([ s[2][i] for i = 1:2:len_s2 ])
    bindings = [ :($(s[2][i]) = $(codegen(s[2][i+1], escape_exceptions = escape_exceptions ∪ syms))) for i = 1:2:len_s2 ]
    coded_s  = map(x -> codegen(x, escape_exceptions = escape_exceptions ∪ syms), s[3:end])
    Expr(:let, Expr(:block, coded_s...), bindings...)
  elseif s[1] == :while
    coded_s = map(x -> codegen(x, escape_exceptions = escape_exceptions), s[2:end])
    Expr(:while, coded_s[1], Expr(:block, coded_s[2:end]...))
  elseif s[1] == :for
    len_s2 = length(s[2])
    syms     = Set{Symbol}([ s[2][i] for i = 1:2:len_s2 ])
    bindings = [ :($(s[2][i]) = $(codegen(s[2][i+1], escape_exceptions = escape_exceptions ∪ syms))) for i = 1:2:len_s2 ]
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
    error("missplaced ~ (splice)")
  elseif s[1] == :splice_seq
    error("missplaced ~@ (splice_seq)")
  elseif s[1] == :quasi
    quasiquote(s[2], escape_exceptions)
  elseif s[1] == :lambda || s[1] == :fn
    assert(length(s) >= 3)
    coded_s = map(x -> codegen(x, escape_exceptions = escape_exceptions ∪ Set{Symbol}(s[2])), s[3:end])
    Expr(:function, Expr(:tuple, s[2]...), Expr(:block, coded_s...))
  elseif s[1] == :defn
    # Note: julia's lambdas are not optimized yet, so we don't define defn as a macro.
    #       this should be revisited later.
    coded_s = map(x -> codegen(x, escape_exceptions = escape_exceptions ∪ Set{Symbol}(s[3])), s[4:end])
    Expr(:function, Expr(:call, esc(s[2]), s[3]...), Expr(:block, coded_s...))
  elseif s[1] == :defmacro
     Expr(:macro, Expr(:call, esc(s[2]), s[3]...),
          begin
            coded_s = map(x -> codegen(x, escape_exceptions = escape_exceptions ∪ Set{Symbol}(s[3])), s[4:end])
            sexpr = Expr(:block, coded_s...) #codegen(s[4], escape_exceptions = escape_exceptions ∪ Set(s[3]))
            :(codegen($sexpr, escape_exceptions = $escape_exceptions ∪ Set{Symbol}($(s[3]))))
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

end # module
