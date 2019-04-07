module JLisp

export @jlisp

# U+275C.
const ESC = :❜  # TODO: Find a better symbol.

isescape(x) = false
isescape(ex::Expr) = ex.head === :call && length(ex.args) == 3 && ex.args[2] === ESC

unescape(arg) = isescape(arg) ? [first(arg.args), last(arg.args)] : [arg]
unescape(args::Tuple) = mapreduce(unescape, vcat, args; init=Any[])
unescape(args...) = unescape(args)

struct SyntaxError <: Exception
    msg::String
end

Base.showerror(io::IO, e::SyntaxError) = print(io, "Syntax error: ", e.msg)

macro se_str(s)
    :(throw(SyntaxError($s)))
end

"""
    @jlisp {your {sexp here}}

Parse and execute some jlisp code.
"""
macro jlisp(ex)
    esc(sexp(ex))
end

# Parses a file or literal code.
parse(io::IO) = parse(read(io, String))
function parse(code::AbstractString)
    block = Expr(:block)
    start = 1
    while true
        ex, next = Meta.parse(code, start)
        start == next && break
        start = next
        push!(block.args, @show(sexp(@show ex)))
    end
    block
end

# Runs some parsed code.
run(exs...) = foreach(ex -> Core.eval(@__MODULE__, ex), exs)

# This catches literals like symbols, numbers, and strings.
sexp(x) = x

# This is the entrypoint for expressions.
# They get split into their head and arguments for dispatch purposes.
sexp(ex::Expr) = sexp(Val(ex.head), ex.args...)

# This catches any expression head that we don't explicitly handle.
sexp(::Val{H}, args...) where H = Expr(H, map(sexp, args)...)

# The only valid :braces expressions are {} and {fun}.
sexp(::Val{:braces}) = :nothing
sexp(::Val{:braces}, arg1) = Expr(:call, sexp(arg1))
sexp(::Val{:braces}, args...) = se"Commas are not allowed inside expressions"

# These are your generic S-expressions.
# They're really just containers for a :row representing a :call or keyword expression,
# and optionally another :row representing keyword arguemnts to a :call.
function sexp(::Val{:bracescat}, arg1::Expr, arg2::Expr=Expr(:row))
    arg1.head === :row || arg2.head === :row || se"Unrecognized expression syntax"
    ex = sexp(arg1)
    kws = kwlist(arg2)
    isempty(kws) || insert!(ex.args, 2, Expr(:parameters, kws...))
    ex
end
sexp(::Val{:bracescat}, arg1, arg2, arg3, args...) =
    se"Only one semicolon is allowed to add keyword arguments"
sexp(::Val{:bracescat}, args...) = se"Unrecognized expression argument"

# These usually usually represent function calls, e.g. {max 1 2}.
# However, keyword arguments to the call are missing,
# since they are found in the containing :bracescat expression.
# The first argument can also be a keyword, e.g. {:if {> {rand} 0.5} 1 2}.
sexp(::Val{:row}, arg1::QuoteNode, args...) = kwexpr(Val(arg1.value), unescape(args)...)
function sexp(::Val{:row}, arg1, args...)
    args = unescape(arg1, args...)
    arg1, args = first(args), args[2:end]
    # In very simple cases, avoid having to use the escape operator.
    infix_arg1 = nothing
    if arg1 isa Expr && arg1.head === :call
        length(arg1.args) == 2 || se"Unrecognized function call syntax"
        arg1, infix_arg1 = arg1.args
    end
    ex = Expr(:call, arg1, map(sexp, args)...)
    infix_arg1 === nothing || insert!(ex.args, 2, sexp(infix_arg1))
    ex
end

# Converts a :row of keyword arguments into a list of :kw expressions.
kwlist(x) = se"Invalid value as keyword arguments"
function kwlist(ex::Expr)
    if ex.head === :(=)
        # Single keyword.
        first(ex.args) isa Symbol ||
            se"Keyword argument name must be an identifier"
        Expr(:kw, first(ex.args), sexp(last(ex.args)))
    elseif ex.head === :row
        # Keyword list.
        map(kwlist, ex.args)
    else
        se"Unrecognized keyword argument syntax"
    end
end

# An assignment, i.e. var = val.
# Example: {:def x 1}.
kwexpr(::Val{:def}, arg1::Symbol, arg2) = Expr(:(=), arg1, sexp(arg2))
kwexpr(::Val{:def}, arg1, arg2) = se"First argument to :def must be an identifier"
kwexpr(::Val{:def}, args...) = se"Wrong number of arguments to :def"

# An if-else expression that does not contain any elseif.
# Example: {:if {fun args} {do this} {otherwise this}}
kwexpr(::Val{:if}, arg1, arg2, arg3=:nothing) =
    Expr(:if, sexp(arg1), sexp(arg2), sexp(arg3))
kwexpr(::Val{:if}, arg1, arg2, arg3, arg4, args...) =
    se"Too many arguments to :if (use :cond for elseif)"
kwexpr(::Val{:if}, args...) = se"Not enough arguments to :if"

# A for loop.
# Example: {:for {i 1:10} {print i}}
function kwexpr(::Val{:for}, arg1::Expr, arg2=:nothing)
    it = sexp(arg1)
    it isa Expr && it.head === :call && length(it.args) == 2 && first(it.args) isa Symbol ||
        se"Invalid iterator given to :for"
    Expr(:for, Expr(:(=), first(it.args), sexp(last(it.args))), sexp(arg2))
end
kwexpr(::Val{:for}, arg1::Expr, args...) = se"Wrong number of arguments to :for"
kwexpr(::Val{:for}, args...) = se"Invalid iterator given to :for"

# A while loop.
# Example: {:while {pred} body}
kwexpr(::Val{:while}, arg1, arg2) = Expr(:while, sexp(arg1), sexp(arg2))
kwexpr(::Val{:while}, args...) = se"Wrong number of arguments to :while"

# A multi-branch if statement, i.e. elseif.
# Example: {:cond {{pred1 x} body1} {{pred2 x} body2} {true body_fallback}}
# Note that no else is inserted.
function kwexpr(::Val{:cond}, args...)
    # TODO
end

# TODO: try/catch/finally, let, defun, defmacro.

end
