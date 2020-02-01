
function funhead(name, args::Vector)
    # Collect positional and keyword arguments.
    pos, kws = [], []
    found_kws = false
    for arg in args
        if arg == QuoteNode(:kw)
            found_kws && se"Can only indicate start of keywords once"
            found_kws = true
        elseif found_kws
            kw = if arg isa Expr && arg.head === :(=)
                Expr(:kw, map(sexp, arg.args)...)
            else
                sexp(arg)
            end
            push!(kws, kw)
        else
            push!(pos, sexp(arg))
        end
    end

    # TODO: This is kinda ugly.
    if name isa Expr && name.head === :where
        call = Expr(:call, sexp(first(name.args)), pos...)
        isempty(kws) || insert!(call.args, 2, Expr(:parameters, kws...))
        Expr(:where, call, last(name.args))
    else
        head = Expr(:call, sexp(name), pos...)
        isempty(kws) || insert!(head.args, 2, Expr(:parameters, kws...))
        head
    end
end

# Create a function definition.
function deffun(ismacro::Bool, name, args::Vector, body...)
    head = funhead(name, args)
    Expr(ismacro ? :macro : :function, head, Expr(:block, map(sexp, body)...))
end


# A return statement.
# Example: {:return 1}
kwexpr(::Val{:return}) = Expr(:return, nothing)
kwexpr(::Val{:return}, arg1) = Expr(:return, sexp(arg1))
kwexpr(::Val{:return}, args...) = se"Invalid return statement"

# A function definition.
# Example: {:function foo {arg1 arg2::Type arg3::Type=default :kw kw1} {body}}
kwexpr(::Val{:function}, args...) = se"Invalid :function definition"
kwexpr(::Val{:function}, arg1, arg2::Expr, args...) = deffun(false, arg1, arg2.args, args...)
