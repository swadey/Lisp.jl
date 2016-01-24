using LispSyntax
using Base.Test

# ----------------------------------------------------------------------------------------------------------------------
# Setup
# ----------------------------------------------------------------------------------------------------------------------
macro incr(x)
  quote
    $(esc(x)) = $(esc(x)) + 1
    $(esc(x))
  end
end

# ----------------------------------------------------------------------------------------------------------------------
# Reader
# ----------------------------------------------------------------------------------------------------------------------
@test LispSyntax.read("1.1f") == 1.1f0
@test LispSyntax.read("1.2f") == 1.2f0
@test LispSyntax.read("2f")   == 2f0

@test LispSyntax.read("3.0d") == 3.0

@test LispSyntax.read("4")    == 4

@test LispSyntax.read("\\u2312") == '\u2312'

@test LispSyntax.read("\\040") == ' '

@test LispSyntax.read("\\c") == 'c'

@test LispSyntax.read("\"test\"") == "test"

@test LispSyntax.read("true") == true
@test LispSyntax.read("false") == false

@test LispSyntax.read("test") == :test

@test LispSyntax.read("()") == sx()
@test LispSyntax.read("(1.1f)") == sx(1.1f0)
@test LispSyntax.read("(1.1f 2.2f)") == sx(1.1f0, 2.2f0)
@test LispSyntax.read("(+ 1.1f 2)") == sx(:+, 1.1f0, 2)
@test LispSyntax.read("(this (+ 1.1f 2))") == sx(:this, sx(:+, 1.1f0, 2))
@test LispSyntax.read("(this (+ 1.1f 2) )") == sx(:this, sx(:+, 1.1f0, 2))

@test LispSyntax.read("#{1 2 3 4}") == Set([1, 2, 3, 4])

@test LispSyntax.read("""#{
                        1 2
                        3 4
                        }""") == Set([1, 2, 3, 4])

@test LispSyntax.read("{a 2 b 3}") == Dict(:a => 2, :b => 3)

@test LispSyntax.read("""{
                        a 2
                        b 3
                        }""") == Dict(:a => 2, :b => 3)

@test LispSyntax.read("[1 2 3 4]")  == sx(1, 2, 3, 4)

@test LispSyntax.read("""[
                        1 2
                        3 4
                        ]""")  == sx(1, 2, 3, 4)

@test LispSyntax.read("[]")         == sx()
@test LispSyntax.read("[1]")        == sx(1)

@test LispSyntax.read("'test")      == sx(:quote, :test)

@test LispSyntax.read("`test")      == sx(:quasi, :test)

@test LispSyntax.read("~test")      == sx(:splice, :test)
@test LispSyntax.read("~@(1 2 3)")  == sx(:splice_seq, sx(1, 2, 3))

@test LispSyntax.read("`~test")     == sx(:quasi, sx(:splice, :test))

@test desx(sx(:splice_seq, sx(1, 2, 3))) == Any[ :splice_seq, [1, 2, 3] ]
@test desx(sx(:splice_seq, sx(1, 2, sx(3)))) == Any[ :splice_seq, Any[ 1, 2, [3] ] ]

@test LispSyntax.read("""(defn multiline
                           [x]
                           (+ x 1))""") == sx(:defn, :multiline, sx(:x), sx(:+, :x, 1))

@test LispSyntax.read("""
(defn f1 [n]
   (if (< n 2)
       1
       (+ (f1 (- n 1))
          (f1 (- n 2)))))
""") == sx(:defn, :f1, sx(:n), sx(:if, sx(:<, :n, 2), 1, sx(:+, sx(:f1, sx(:-, :n, 1)), sx(:f1, sx(:-, :n, 2)))))

assign_reader_dispatch(:sx, x -> sx(x.vector...))
assign_reader_dispatch(:hash, x -> [ x.vector[i] => x.vector[i+1] for i = 1:2:length(x.vector) ])
@test LispSyntax.read("#sx[a b c]") == sx(:a, :b, :c)
@test LispSyntax.read("#sx [ 1 2 3 ]") == sx(1, 2, 3)

# ----------------------------------------------------------------------------------------------------------------------
# Code generation
# ----------------------------------------------------------------------------------------------------------------------
@test codegen(desx(LispSyntax.read("(if true a)"))) == :(true && $(esc(:a)))
@test codegen(desx(LispSyntax.read("(if true a b)"))) == :(true ? $(esc(:a)) : $(esc(:b)))

@test codegen(desx(LispSyntax.read("(call)"))) == :($(esc(:call))())
@test codegen(desx(LispSyntax.read("(call a)"))) == :($(esc(:call))($(esc(:a))))
@test codegen(desx(LispSyntax.read("(call a b)"))) == :($(esc(:call))($(esc(:a)), $(esc(:b))))
@test codegen(desx(LispSyntax.read("(call a b c)"))) == :($(esc(:call))($(esc(:a)), $(esc(:b)), $(esc(:c))))

@test codegen(desx(LispSyntax.read("(lambda (x) (call x))"))) == Expr(:function, :((x,)), Expr(:block, :($(esc(:call))(x))))
@test codegen(desx(LispSyntax.read("(def x 3)"))) == :($(esc(:x)) = 3)
@test codegen(desx(LispSyntax.read("(def x (+ 3 1))"))) == :($(esc(:x)) = $(esc(:+))(3, 1))

@test codegen(desx(LispSyntax.read("test"))) == :($(esc(:test)))
@test codegen(desx(LispSyntax.read("'test"))) == QuoteNode(:test)
@test codegen(desx(LispSyntax.read("'(1 2)"))) == :(construct_sexpr(1, 2))
@test codegen(desx(LispSyntax.read("'(1 x)"))) == :(construct_sexpr(1, :x))
@test codegen(desx(LispSyntax.read("'(1 (1 2))"))) == :(construct_sexpr(1, construct_sexpr(1, 2)))
@test codegen(desx(LispSyntax.read("'(1 (test x))"))) == :(construct_sexpr(1, construct_sexpr(:test, :x)))
@test codegen(desx(LispSyntax.read("(call 1 '2)"))) == :($(esc(:call))(1, 2))

# ----------------------------------------------------------------------------------------------------------------------
# Scope and variables
# ----------------------------------------------------------------------------------------------------------------------
global x = 10
@test @lisp("x") == 10
@test lisp"x" == 10

lisp"(def w (+ 3 1))"
@test w == 4

# ----------------------------------------------------------------------------------------------------------------------
# Quoting and splicing
# ----------------------------------------------------------------------------------------------------------------------
@test @lisp("`~x") == 10
@test lisp"`~x" == 10
@test lisp"'test" == :test
@test lisp"'(1 2)" == Any[1, 2]
@test lisp"'(1 x)" == Any[1, :x]
@test lisp"'(1 (1 2))" == Any[1, Any[1, 2]]
@test lisp"'(1 (test x))" == Any[1, Any[:test, :x]]
@test @lisp("`(test ~x)") == Any[ :test, 10 ]
@test lisp"`(test ~x)" == Any[ :test, 10 ]
@test @lisp("`(~x ~x)") == Any[ 10, 10 ]
global y = Any[ 1, 2 ]
@test @lisp("`(~x ~@y)") == Any[ 10, 1, 2 ]
@test @lisp("`(~x ~y)") == Any[ 10, Any[1, 2] ]

@test @lisp("`(10 ~(+ 10 x))") == Any[10, 20]

@test lisp"(quote (+ 1 2))" == Any[:+, 1, 2]

# ----------------------------------------------------------------------------------------------------------------------
# Functions
# ----------------------------------------------------------------------------------------------------------------------
@lisp("(defn xxx [a b] (+ a b))")
@test @lisp("(xxx 1 2)") == 3

global z = 10
@lisp("(defn yyy [a] (+ a z))")
@test @lisp("(yyy 1)") == 11
@test @lisp("(yyy z)") == 20

# recursion
lisp"(defn fib [a] (if (< a 2) a (+ (fib (- a 1)) (fib (- a 2)))))"
@test lisp"(fib 2)" == 1
@test lisp"(fib 4)" == 3
@test lisp"(fib 30)" == 832040
@test lisp"(fib 40)" == 102334155

# Note this version is very slow due to the anonymous function
lisp"(def fib2 (lambda [a] (if (< a 2) a (+ (fib2 (- a 1)) (fib2 (- a 2))))))"
@test lisp"(fib2 2)" == 1
@test lisp"(fib2 4)" == 3
@test lisp"(fib2 30)" == 832040

lisp"(defn dostuff [a] (@incr a) (@incr a) (@incr a))"
@test lisp"(dostuff 3)" == 6
@test lisp"(dostuff 6)" == 9

lisp"(def dostuff2 (lambda [a] (@incr a) (@incr a) (@incr a)))"
@test lisp"(dostuff2 3)" == 6
@test lisp"(dostuff2 6)" == 9

lisp"(def dostuff3 (fn [a] (@incr a) (@incr a) (@incr a)))"
@test lisp"(dostuff3 3)" == 6
@test lisp"(dostuff3 6)" == 9
@test lisp"((lambda [x] (+ x 1)) 5)" == 6
@test lisp"#{1 2 z}" == Set([1, 2, 10])
@test lisp"{1 2 2 z}" == Dict(1 => 2, 2 => 10)
@test lisp"#sx[+ 1 2]" == 3
@test lisp"#hash['+ 1 '- z]" == Dict(:+ => 1, :- => 10)

# ----------------------------------------------------------------------------------------------------------------------
# Macros
# ----------------------------------------------------------------------------------------------------------------------
lisp"(defn fact [a] (if (< a 1) 1 (* a (fact (- a 1)))))"
lisp"(defmacro fapply [f a] `(~f ~a))"
@test @fapply(fib2, 2) == 1
@test @fapply(fact, 3 + 1) == 24
@test lisp"(@fapply fib2 2)" == 1
@test lisp"(@fapply fact (+ 3 1))" == 24

fcount = 0
lisp"(defmacro fapply_trace [f a] (global fcount) (@incr fcount) `(~f ~a))"
@test @fapply_trace(fib2, 2) == 1
@test fcount == 1
@test @fapply_trace(fact, 3 + 1) == 24
@test fcount == 2

# ----------------------------------------------------------------------------------------------------------------------
# Loops
# ----------------------------------------------------------------------------------------------------------------------
number = 0
output = 0

lisp"(while (< number 2) (@incr number) (@incr output))"
@test number == 2
@test output == 2
r = output
lisp"(for [i (range 1 10)] (@incr r))"
@test r == 12

r = 0
lisp"(for [i (range 1 10) j (range 1 10)] (@incr r))"
@test r == 100

# ----------------------------------------------------------------------------------------------------------------------
# Let and do
# ----------------------------------------------------------------------------------------------------------------------
@test lisp"(let [x 10] x)" == 10
@test lisp"(let [x 10 y 20] (+ x y))" == 30
@test lisp"(let [x 10 y 20 z 20] (+ x y z))" == 50
@test lisp"(let [x 10 y 20 z 20] (+ x y z number))" == 52
@test lisp"(let [x 10 y 20 z 20 number 10] (+ x y z number))" == 60
@test lisp"(let [x 10 y 20 z 20] (- (+ x y z number) output))" == 50

lisp"(do (@incr r) (@incr number))"
@test number == 3
@test r == 101

# ----------------------------------------------------------------------------------------------------------------------
# Import
# ----------------------------------------------------------------------------------------------------------------------
lisp"(import ParserCombinator)"
@test lisp"(@E_str \"S\")" == E"S"

# ----------------------------------------------------------------------------------------------------------------------
# Bug reports
# ----------------------------------------------------------------------------------------------------------------------
@test lisp"""(def game_map (Dict
          (=> 'living_room
              '((you are in the living room
                 of a wizards house - there is a wizard
                 snoring loudly on the couch -)
                (west door garden)
                (upstairs stairway attic)))))""" == Dict(:living_room =>
                                                         Any[ Any[ :you, :are, :in, :the, :living, :room, :of, :a, :wizards, :house, :-,
                                                                   :there, :is, :a, :wizard, :snoring, :loudly, :on, :the, :couch, :- ],
                                                              Any[ :west, :door, :garden ],
                                                              Any[ :upstairs, :stairway, :attic ] ])

