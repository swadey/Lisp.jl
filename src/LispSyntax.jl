__precompile__()

module LispSyntax

include("parser.jl")

export sx, desx, codegen, @lisp, @lisp_str


#=
  Internal types
=#
type SExpr
    vector::Vector{Any}
end

sx(x...) = SExpr([x...])

==(a :: SExpr, b :: SExpr) = a.vector == b.vector

desx(s) = s
desx(s::SExpr) = map(desx, s.vector)

lispify(s) = string(s)
lispify(s::SExpr) = string("(", join(map(lispify, s.vector), " "), ")")

"Convert the input tuple to an array."
construct_sexpr(items...) = collect(items)

quasiquote(s, esc_except::Set{Symbol}) = s
quasiquote(s::Symbol, esc_except::Set{Symbol}) = Expr(:quote, s)

function quasiquote(s::Vector{Any}, esc_except::Set{Symbol})
    if length(s) == 2 && first(s) == :splice
        codegen(s[2], esc_except)
    elseif length(s) == 2 && first(s) == :splice_seq
        Expr(:..., codegen(s[2], esc_except))
    else
        Expr(:call, :construct_sexpr, map(s -> quasiquote(s, esc_except), s)...)
    end
end

quote_it(s) = s
quote_it(s::Symbol) = QuoteNode(s)
quote_it(s::Vector{Any}) = Expr(:call, :construct_sexpr, map(s -> quote_it(s), s)...)

codegen(s, esc_except = Set{Symbol}()) = s
codegen(s::Symbol, esc_except = Set{Symbol}()) = s in esc_except ? s : esc(s)

function codegen(s::Vector{Any}, esc_except = Set{Symbol}())
    length(s) == 0 && return s # empty array
    codegen(first(s), s, esc_except)
end

codegen(x::Symbol, s::Vector{Any}, esc_except::Set{Symbol}) = codegen(Val{x}, s, esc_except)

function codegen(x::Type{Val{:if}}, s::Vector{Any}, esc_except::Set{Symbol})
    if length(s) == 3
        :($(codegen(s[2], esc_except)) && $(codegen(s[3], esc_except)))
    elseif length(s) == 4
        :($(codegen(s[2], esc_except)) ? 
          $(codegen(s[3], esc_except)) : 
          $(codegen(s[4], esc_except)))
    else
        error("illegal if statement $s")
    end
end

function codegen(x::Type{Val{:def}}, s::Vector{Any}, esc_except::Set{Symbol})
    @assert length(s) == 3
    :($(esc(s[2])) = $(codegen(s[3], esc_except)))
end

function codegen(x::Type{Val{:let}}, s::Vector{Any}, esc_except::Set{Symbol})
    n = length(s[2])
    syms = Set{Symbol}([s[2][i] for i = 1:2:n])
    bindings = [:($(s[2][i]) = $(codegen(s[2][i+1], esc_except ∪ syms))) for i = 1:2:n]
    coded_s = map(x -> codegen(x, esc_except ∪ syms), s[3:end])
    Expr(:let, Expr(:block, coded_s...), bindings...)
end

function codegen(x::Type{Val{:while}}, s::Vector{Any}, esc_except::Set{Symbol})
    coded_s = map(x -> codegen(x, esc_except), s[2:end])
    Expr(:while, coded_s[1], Expr(:block, coded_s[2:end]...))
end

function codegen(x::Type{Val{:for}}, s::Vector{Any}, esc_except::Set{Symbol})
    n = length(s[2])
    syms = Set{Symbol}([s[2][i] for i = 1:2:n])
    bindings = [:($(s[2][i]) = $(codegen(s[2][i+1], esc_except ∪ syms))) for i = 1:2:n]
    coded_s = map(x -> codegen(x, esc_except ∪ syms), s[3:end])
    Expr(:for, Expr(:block, bindings...), Expr(:block, coded_s...))
end

function codegen(x::Type{Val{:do}}, s::Vector{Any}, esc_except::Set{Symbol})
    Expr(:block, map(x -> codegen(x, esc_except), s[2:end])...)
end

function codegen(x::Type{Val{:global}}, s::Vector{Any}, esc_except::Set{Symbol})
    Expr(:global, map(x -> esc(x), s[2:end])...)
end

codegen(x::Type{Val{:quote}}, s::Vector{Any}, esc_except::Set{Symbol}) = quote_it(s[2])

function codegen(x::Type{Val{:import}}, s::Vector{Any}, esc_except::Set{Symbol})
    Expr(:using, map(x -> esc(x), s[2:end])...)
end

function codegen(x::Type{Val{:splice}}, s::Vector{Any}, esc_except::Set{Symbol})
    error("missplaced ~ (splice)")
end

function codegen(x::Type{Val{:splice_seq}}, s::Vector{Any}, esc_except::Set{Symbol})
    error("missplaced ~@ (splice_seq)")
end

function codegen(x::Type{Val{:quasi}}, s::Vector{Any}, esc_except::Set{Symbol})
    quasiquote(s[2], esc_except)
end

function codegen(x::Type{Val{:lambda}}, s::Vector{Any}, esc_except::Set{Symbol})
    @assert length(s) ≥ 3
    coded_s = map(x -> codegen(x, esc_except ∪ Set{Symbol}(s[2])), s[3:end])
    Expr(:function, Expr(:tuple, s[2]...), Expr(:block, coded_s...))
end

codegen(x::Type{Val{:fn}}, s::Vector{Any}, esc_except::Set{Symbol}) = codegen(:lambda, s, esc_except)
codegen(x::Type{Val{:λ}}, s::Vector{Any}, esc_except::Set{Symbol}) = codegen(:lambda, s, esc_except)

function codegen(x::Type{Val{:defn}}, s::Vector{Any}, esc_except::Set{Symbol})
    # Note: julia's lambdas are not optimized yet, so we don't define defn as a macro.
    #       this should be revisited later.
    coded_s = map(x -> codegen(x, esc_except ∪ Set{Symbol}(s[3])), s[4:end])
    Expr(:function, Expr(:call, esc(s[2]), s[3]...), Expr(:block, coded_s...))
end

function codegen(x::Type{Val{:defmacro}}, s::Vector{Any}, esc_except::Set{Symbol})
    Expr(:macro, Expr(:call, esc(s[2]), s[3]...),
        begin
            coded_s = map(x -> codegen(x, esc_except ∪ Set{Symbol}(s[3])), s[4:end])
            sexpr = Expr(:block, coded_s...) # codegen(s[4], esc_except ∪ Set(s[3]))
            :(codegen($sexpr, $esc_except ∪ Set{Symbol}($(s[3]))))
        end
    )
end

# TODO
codegen(x::Type{Val{:defmethod}}, s::Vector{Any}, esc_except::Set{Symbol}) = nothing

function codegen(x::Any, s::Vector{Any}, esc_except::Set{Symbol})
    coded_s = map(x -> codegen(x, esc_except), s)
    coded_t = typeof(coded_s[1])
    if coded_t == Symbol && ismatch(r"^@.*$", string(coded_s[1])) ||
       coded_t == Expr && ismatch(r"^@.*$", string(coded_s[1].args[1]))
        return Expr(:macrocall, coded_s[1], coded_s[2:end]...)
    else
        Expr(:call, coded_s[1], coded_s[2:end]...)
    end
end

# TODO: lexpr needs to be fixed prior to exposure
lexpr(s::AbstractString) = codegen(desx(LispSyntax.read(s)))

macro lisp(s)
    lexpr(s)
end

macro lisp_str(s)
    lexpr(s)
end


end # module
