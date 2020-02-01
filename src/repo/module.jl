# Creates a module definition.
defmodule(isbare::Bool, name, args...) =
    Expr(:module, !isbare, sexp(name), Expr(:block, map(sexp, args)...))


# A module definition.
# Example: {:module Foo {body} {morebody}}
kwexpr(::Val{:module}, arg1, args...) = defmodule(false, arg1, args...)

# A bare module definition (same syntax as :module).
kwexpr(::Val{:baremodule}, arg1, args...) = defmodule(true, arg1, args...)
