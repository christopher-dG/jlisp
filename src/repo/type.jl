# An abstract type definition.
# Example: {:abstract Foo{T} <: Super}
kwexpr(::Val{:abstract}, args...) = Expr(:abstract, map(sexp, args)...)

# A primitive type definition.
# Example: {:primitive Foo <: Super 32}
kwexpr(::Val{:primitive}, args...) = Expr(:primitive, map(sexp, args)...)
