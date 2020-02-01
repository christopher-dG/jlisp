
# An if-else expression that does not contain any elseif.
# Example: {:if {fun args} {do this} {otherwise this}}
kwexpr(::Val{:if}, args...) = se"Not enough arguments to :if"
kwexpr(::Val{:if}, arg1, arg2, arg3, arg4, args...) =
    se"Too many arguments to :if (use :cond for elseif)"
function kwexpr(::Val{:if}, arg1, arg2, arg3=nothing)
    ex = Expr(:if, sexp(arg1), sexp(arg2))
    arg3 === nothing || push!(ex.args, sexp(arg3))
    ex
end


# A multi-branch if statement, i.e. elseif.
# No else is inserted.
# Example: {:cond {{pred1 x} body1} {{pred2 x} body2} {true body3}}
kwexpr(::Val{:cond}) = se":cond must have at least one branch"
function kwexpr(::Val{:cond}, arg1::Expr, args...)
    arg1.head === :row && length(arg1.args) == 2 || se"Invalid :cond branch"
    ex = Expr(:if, sexp(first(arg1.args)), Expr(:block, sexp(last(arg1.args))))
    branch = ex
    for arg in args
        arg isa Expr && arg.head === :row && length(arg.args) == 2 ||
            se"Invalid :cond branch"
        elif = Expr(:elseif, sexp(first(arg.args)), Expr(:block, sexp(last(arg.args))))
        push!(branch.args, elif)
        branch = elif
    end
    ex
end
