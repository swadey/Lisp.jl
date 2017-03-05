using ParserCombinator, Compat

const reader_table = Dict{Symbol, Function}()

const expr         = Delayed()
const floaty_dot   = p"[-+]?[0-9]*\.[0-9]+([eE][-+]?[0-9]+)?[Ff]" > (x -> parse(Float32, x[1:end-1]))
const floaty_nodot = p"[-+]?[0-9]*[0-9]+([eE][-+]?[0-9]+)?[Ff]" > (x -> parse(Float32, x[1:end-1]))
const floaty       = floaty_dot | floaty_nodot
const white_space  = p"([\s\n\r]*(?<!\\);[^\n\r$]+[\n\r\s$]*|[\s\n\r]+)"
const opt_ws       = white_space | e""

const doubley      = p"[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?[dD]" > (x -> parse(Float64, x[1:end-1]))

const inty         = p"[-+]?\d+" > (x -> parse(Int, x))

const uchary       = p"\\(u[\da-fA-F]{4})" > (x -> begin y = unescape_string(x); y[chr2ind(y, 1)] end)
const achary       = p"\\[0-7]{3}" > (x -> unescape_string(x)[1])
const chary        = p"\\." > (x -> x[2])

const stringy      = p"(?<!\\)\".*?(?<!\\)\"" > (x -> x[2:end-1]) #_0[2:end-1] } #r"(?<!\\)\".*?(?<!\\)"
const booly        = p"(true|false)" > (x -> x == "true" ? true : false)
const symboly      = p"[^\d(){}#'`,@~;~\[\]^\s][^\s()#'`,@~;^{}~\[\]]*" > symbol
const macrosymy    = p"@[^\d(){}#'`,@~;~\[\]^\s][^\s()#'`,@~;^{}~\[\]]*" > symbol

const sexpr        = E"(" + ~opt_ws + Repeat(expr + ~opt_ws) + E")" |> (x -> SExpr(x))
const hashy        = E"#{" + ~opt_ws + Repeat(expr + ~opt_ws) + E"}" |> (x -> Set(x))
const curly        = E"{" + ~opt_ws + Repeat(expr + ~opt_ws) + E"}" |> (x -> [ x[i] => x[i+1] for i = 1:2:length(x) ])
const dispatchy    = E"#" + symboly + ~opt_ws + expr |> (x -> reader_table[x[1]](x[2]))
const bracket      = E"[" + ~opt_ws + Repeat(expr + ~opt_ws) + E"]" |> (x -> SExpr(x)) # TODO: not quite right
const quot         = E"'" + expr > (x -> sx(:quote, x))
const quasi        = E"`" + expr > (x -> sx(:quasi, x))
const tildeseq     = E"~@" + expr > (x -> sx(:splice_seq, x))
const tilde        = E"~" + expr > (x -> sx(:splice, x))

const syntax = [doubley, floaty, inty, uchary, achary, chary, stringy, booly, symboly, macrosymy,
                sexpr, hashy, curly, bracket, quot, quasi, tildeseq, tilde]

expr.matcher = Nullable{ParserCombinator.Matcher}(reduce(|, syntax))

function read(str)
  x = parse_one(str, expr)
  x[1]
end
