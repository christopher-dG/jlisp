
# This catches literals like symbols, numbers, and strings.
sexp(x) = x

# This is the entrypoint for expressions.
# They get flattened, then split into their head and arguments for dispatch purposes.
function sexp(ex::Expr; flatten::Bool=false)
    flatten && flatten!(ex)
    sexp(Val(ex.head), ex.args...)
end

# This catches any expression head that we don't explicitly handle.
sexp(::Val{H}, args...) where H = Expr(H, map(sexp, args)...)

# Valid :braces expressions are: {}, {fun}, {var val}, {:kw}, and {@macro arg1 arg2}.
sexp(::Val{:braces}, args...) = se"Commas are not allowed inside expressions"
sexp(::Val{:braces}) = :nothing
sexp(::Val{:braces}, arg1::QuoteNode) = kwexpr(Val(arg1.value))
sexp(::Val{:braces}, arg1::Symbol) = Expr(:call, arg1)
function sexp(::Val{:braces}, arg1::Expr)
    if arg1.head === :macrocall
        Expr(:macrocall, map(sexp, arg1.args)...)
    elseif arg1.head === :call && length(arg1.args) == 2 && first(arg1.args) isa Symbol
        Expr(:call, first(arg1.args), sexp(last(arg1.args)))
    else
        se"Unrecognized function call syntax"
    end
end

# These are your generic S-expressions.
# They can be parsed as function calls, e.g. {max 1 2},
# or keyword expressions, e.g. {:if {isodd {rand Int}} 1 2}.
sexp(::Val{:row}) = se"Unrecognized expression"
sexp(::Val{:row}, arg1::QuoteNode, args...) = kwexpr(Val(arg1.value), args...)
function sexp(::Val{:row}, arg1, args...)
    # In very simple cases, avoid having to escape an operator.
    infix_arg1 = nothing
    if arg1 isa Expr && arg1.head === :call
        length(arg1.args) == 2 || se"Unrecognized function call syntax"
        arg1, infix_arg1 = arg1.args
    end

    # Collect keyword arguments.
    kwstart = nothing
    for (i, arg) in enumerate(args)
        if arg isa Expr && arg.head === :(=)
            length(arg.args) == 2 && first(arg.args) isa Symbol ||
                se"Invalid keyword argument syntax"
            kwstart === nothing && (kwstart = i)
        elseif kwstart !== nothing
            se"Keyword arguments must occur at the end of the expression"
        end
    end
    kw = if kwstart !== nothing
        kws = collect(args[kwstart:end])
        args = args[1:kwstart-1]
        map!(ex -> Expr(:kw, first(ex.args), sexp(last(ex.args))), kws, kws)
        Expr(:parameters, kws...)
    end

    ex = Expr(:call, sexp(arg1), map(sexp, args)...)
    infix_arg1 === nothing || insert!(ex.args, 2, sexp(infix_arg1))
    kw === nothing || insert!(ex.args, 2, kw)
    ex
end
