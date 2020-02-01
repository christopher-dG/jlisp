
# Creates a struct definition.
defstruct(ismutable::Bool, head, args...) =
    Expr(:struct, ismutable, sexp(head), Expr(:block, map(sexp, args)...))


# A struct definition.
# Example: {:struct Foo field {field2 Type} {:function Foo {} {new 0 0}}}
kwexpr(::Val{:struct}, arg1, args...) = defstruct(false, arg1, args...)

# A mutable struct definition (same syntax as :struct).
kwexpr(::Val{:mutable}, arg1, args...) = defstruct(true, arg1, args...)
