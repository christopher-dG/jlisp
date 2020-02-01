# A for loop.
# Example: {:for {i 1:10} {println 2i}}
kwexpr(::Val{:for}, args...) = se"Wrong number of arguments to :for"
kwexpr(::Val{:for}, arg1) = se"Invalid :for iterator"
function kwexpr(::Val{:for}, arg1::Expr, args...)
    # Allow shortcut syntax for a single iterator.
    arg1.args[1] isa Expr || (arg1 = Expr(:row, arg1))
    its = map(sexp, arg1.args)
    !isempty(its) && all(isbinding, its) || se"Invalid :for iterator"
    binds = map(ex -> Expr(:(=), first(ex.args), sexp(last(ex.args))), its)
    ex = Expr(:for, Expr(:block, binds...))
    isempty(args) || append!(ex.args, map(sexp, args))
    ex
end
