# Add a :catch or :finally expression to a :try expression.
function tryex!(ex::Expr, arg::Expr)
    arg.head === :row || se"Invalid :try expression"
    if first(arg.args) == QuoteNode(:catch)
        length(arg.args) in (2, 3) || se"Invalid :catch expression"
        ex.args[3] === false || se"Cannot repeat :catch expression"
        ex.args[3] = Expr(:block, sexp(last(arg.args)))
        length(arg.args) == 3 && (ex.args[2] = sexp(arg.args[2]))
    elseif first(arg.args) == QuoteNode(:finally)
        length(arg.args) == 2 || se"Invalid :finally expression"
        length(ex.args) < 4 || se"Cannot repeat :finally expression"
        push!(ex.args, Expr(:block, sexp(last(arg.args))))
    end
    ex
end


# A try/catch/finally expression.
# Example: {:try {foo} {:catch ex {bar ex}} {:finally {baz}}}
kwexpr(::Val{:try}, args...) = se"Wrong number of arguments to :try"
function kwexpr(::Val{:try}, arg1, arg2::Expr, arg3=nothing)
    ex = Expr(:try, Expr(:block, sexp(arg1)), false, false)
    tryex!(ex, arg2)
    arg3 === nothing || tryex!(ex, arg3)
    ex
end
