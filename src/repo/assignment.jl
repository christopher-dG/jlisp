
# An assignment, i.e. var = val.
# Example: {:def x 1}
kwexpr(::Val{:def}, args...) = se"Wrong number of arguments to :def"
kwexpr(::Val{:def}, arg1, arg2) = Expr(:(=), arg1, sexp(arg2))

# A const assignment.
# Example: {:const x 1}
kwexpr(::Val{:const}, args...) = se"Wrong number of arguments to :const"
kwexpr(::Val{:const}, arg1, arg2) = Expr(:const, kwexpr(Val(:def), arg1, arg2))
