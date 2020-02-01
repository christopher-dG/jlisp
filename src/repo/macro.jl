
# A macro definition.
# Example: {:macro foo {arg1 arg2 arg3::Expr}}
kwexpr(::Val{:macro}, args...) = se"Invalid :macro definition"
kwexpr(::Val{:macro}, arg1, arg2::Expr, args...) = deffun(true, arg1, arg2.args, args...)
