LispSyntax.jl: A clojure-like lisp syntax for julia
===================================================

[![Join the chat at https://gitter.im/swadey/LispSyntax.jl](https://badges.gitter.im/swadey/LispSyntax.jl.svg)](https://gitter.im/swadey/LispSyntax.jl?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
![Build Status](https://travis-ci.org/swadey/LispSyntax.jl.svg?branch=master)

This package provides a julia-to-lisp syntax translator with
convenience macros that let you do this: 

```julia 
lisp"(defn fib [a] (if (< a 2) a (+ (fib (- a 1)) (fib (- a 2)))))" 
@test lisp"(fib 30)" == 832040 
@test fib(30)        == 832040 
```

LispSyntax.jl is implemented as an expression translator between
lisp/clojure-like syntax and julia's AST.  Julia's compiler, JIT and
multiple-dispatch infrastructure is used for code generation and
execution. Because of this, LispSyntax.jl is not really clojure or lisp in
most meaningful ways.  The semantics are entirely julia-based (which
are very similar to scheme/lisp in many ways).  The net result is that
LispSyntax.jl is really an alternative S-expression-like syntax for julia,
not an implemention of clojure or lisp.

Special Forms
-------------

- `(def symbol init)`
- `(quote form)`
- `(defn symbol [param*] expr*)`
- `(defmacro symbol [param*] expr*)`
- `(lambda [param*] expr*)`
- `(fn [param*] expr*)`
- `(λ [param*] expr*)`
- `(let [binding*] expr*)`
- `(global symbol*)`
- `(while test expr*)`
- `(for [binding*] expr*)`
- `(import package*)`


Notable Differences
-------------------

- *Symbol names cannot have -, \*, /, ? ...* - Julia symbol naming is used for
   everything, as a result, Julia syntax restrictions are maintained
   in `LispSyntax.jl`.
- *Reference to global variables in function scopes* - Julia requires
   declaration of global symbols that are referenced in function
   scope.  Because of this functions need to declare which symbols are
   global.  This is done via the special form `(global symbol*)`.
- *Binding forms not implemented* - Clojure has very awesome
   destructuring binds that can used in most special forms requiring
   bindings (e.g. `let`, `fn` parameter lists, etc.).  This is not
   currently implemented.
- *Lack of loop/recur* - Currently, this is not implemented.  As with
   Clojure, julia does not currently support TCO, so something like
   this may be needed (but a macro-implementation of tail call rewriting may be
   more appropriate for julia).
- *Optional typing* - Currently not implemented.
- *Method definition* - Also not currently implemented.  If
   implemented it will probably not be a full implementation of
   Clojure's sophisticated dispatch system.
- *Macros differences* - Macros defined in `LispSyntax.jl` look like
   standard Lisp macros but because expressions are special objects in
   julia, S-expressions returned from macros require a special
   translation step to generate julia expression trees.  The result is
   that `LispSyntax.jl` macros are directly translated into Julia macros and
   must be called via special syntax (e.g. `(@macro expr)`).
- *Julia's string macro dispatch not supported (yet)* - for macros
   like `@r_str` which in Julia can be called via `r""`, it is
   currently necessary to call these via standard macro syntax:
   `(@r_str "string")`

TODO
----

- Support for exceptions: this is straight forward but not currently implemented.
- Optional typing to support method definition
- Structs and aggregate types
- Special dispatch for string macro forms
- Modules
- import vs. using vs. include -- only `using` is currently
  implemented and confusingly, it matches Clojure's import form.
- varargs and named arguments
