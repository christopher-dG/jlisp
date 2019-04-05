module JLisp

export @jlisp

const V = Vector

# https://unicode-table.com/en/275C/
const ESC = :❜  # TODO: Find a better symbol.

isescape(x) = false
isescape(ex::Expr) = ex.head === :call && length(ex.args) === 3 && ex.args[2] === ESC
unescape(arg) = isescape(arg) ? [first(arg.args), last(arg.args)] : [arg]
unescape(args::V) = mapreduce(unescape, vcat, args)

"""
A JLisp syntax error.
`msg` contains an error message, and `x` contains what caused it.
"""
struct SyntaxError <: Exception
    msg::String
    x
end

function Base.showerror(io::IO, e::SyntaxError)
    print(io, "SyntaxError: $(e.msg)\nValue: ")
    dump(io, e.x; maxdepth=1)
end

"""
    @jlisp {your {sexp here}}

Parse and execute some JLisp code.
"""
macro jlisp(ex)
    esc(sexp(ex))
end

# Parses a file or literal code.
parse(io::IO) = parse(read(io, String))
function parse(code::String)
    exs = []
    start = 1
    while true
        ex, next = Meta.parse(code, start)
        start == next && break
        start = next
        push!(exs, sexp(ex))
    end
    exs
end

# Runs some parsed code.
run(exs::Vector) = foreach(ex -> Core.eval(@__MODULE__, ex), exs)

# This catches literals like symbols, numbers, and strings.
sexp(x) = x

# This is the entrypoint for expressions.
# They get split into their head and arguments for dispatch purposes.
sexp(ex::Expr) = sexp(Val(ex.head), ex.args)

# This catches any expression head that we don't explicitly handle.
sexp(::Val{H}, args::V) where H = Expr(H, map(sexp, args)...)

# The only valid :braces expressions are {} and {fun}.
function sexp(::Val{:braces}, args::V)
    if length(args) > 1
        msg = "Commas are not allowed inside expressions"
        throw(SyntaxError(msg, Expr(:braces, args...)))
    elseif length(args) == 1
        Expr(:call, sexp(first(args)))
    else
        :nothing
    end
end

# These are your generic S-expressions.
# They're really just containers for a :row representing a :call or keyword expression,
# and optionally another :row representing keyword arguemnts to a :call.
function sexp(::Val{:bracescat}, args::V)
    if length(args) > 2
        msg = "Only one semicolon is allowed to indicate keyword arguments"
        throw(SyntaxError(msg, Expr(:bracescat, args...)))
    elseif !all(ex -> ex isa Expr && ex.head === :row, args)
        msg = "Unrecognized :bracescat expression argument"
        throw(SyntaxError(msg, Expr(:bracescat, args...)))
    end
    ex = sexp(first(args))
    kws = kwlist(length(args) == 2 ? last(args) : Expr(:row))
    isempty(kws) || insert!(ex.args, 2, Expr(:parameters, kws...))
    ex
end

# These usually usually represent function calls, e.g. {max 1 2}.
# However, keyword arguments to the call are missing,
# since they are found in the containing :bracescat expression.
# The first argument can also be a keyword, e.g. {:if {> {rand} 0.5} 1 2}.
function sexp(::Val{:row}, args::V)
    args = unescape(args)
    hd, args = peel(args)
    if hd isa QuoteNode
        # Keyword expression.
        kwexpr(Val(hd.value), args)
    else
        # In very simple cases, avoid having to use the escape operator.
        infix_arg1 = nothing
        if hd isa Expr && hd.head === :call
            length(hd.args) == 2 ||
                throw(SyntaxError("Unrecognized function call syntax", Expr(:row, args...)))
            hd, infix_arg1 = hd.args
        end
        ex = Expr(:call, hd, map(sexp, args)...)
        infix_arg1 === nothing || insert!(ex.args, 2, sexp(infix_arg1))
        ex
    end
end

# Converts a :row of keyword arguments into a list of :kw expressions.
kwlist(x) = throw(SyntaxError("Invalid value as keyword arguments", x))
function kwlist(ex::Expr)
    if ex.head === :(=)
        # Single keyword.
        first(ex.args) isa Symbol ||
            throw(SyntaxError("Keyword argument name must be an identifier", ex))
        Expr(:kw, first(ex.args), sexp(last(ex.args)))
    elseif ex.head === :row
        # Keyword list.
        map(kwlist, ex.args)
    else
        throw(SyntaxError("Unrecognized keyword argument syntax", ex))
    end
end

# An assignment, i.e. var = val.
# Example: {:def x 1}.
function kwexpr(::Val{:def}, args::V)
    ex = Expr(:row, :(:def), args...)
    length(args) == 2 || throw(SyntaxError("Wrong number of arguments to :def", ex))
    first(args) isa Symbol ||
        throw(SyntaxError("First argument to :def must be an identifier", ex))
    Expr(:(=), first(args), sexp(last(args)))
end

# An if-else expression that does not contain any elseif.
# Example: {:if {fun args} truebody falsebody}
function kwexpr(::Val{:if}, args::V)
    ex = Expr(:row, :(:if), args...)
    length(args) < 2 && throw(SyntaxError("Not enough arguments to :if", ex))
    length(args) > 4 &&
        throw(SyntaxError("Too many arguments to :if (use :cond for elseif)", ex))
    Expr(:if, map(sexp, args)...)
end

# A for loop.
# Example: {:for {i 1:10} {print i}}
function kwexpr(::Val{:for}, args::V)
    ex = Expr(:row, :(:for), args...)
    length(args) == 2 || throw(SyntaxError("Wrong number of arguments to :for", ex))
    it = sexp(first(args))
    it isa Expr && it.head === :call && length(it.args) == 2 && first(it.args) isa Symbol ||
        throw(SyntaxError("Invalid iterator specification in :for", ex))
    Expr(:for, Expr(:(=), first(it.args), sexp(last(it.args))), sexp(last(args)))
end

# A while loop.
# Example: {:while {pred} body}
function kwexpr(::Val{:while}, args::V)
    ex = Expr(:row, :(:while), args...)
    length(args) == 2 || throw(SyntaxError("Wrong number of arguments to :while", ex))
    Expr(:while, map(sexp, args)...)
end

# A multi-branch if statement, i.e. elseif.
# Example: {:cond {{pred1 x} body1} {{pred2 x} body2} {true body_fallback}}
# Note that no else is inserted.
function kwexpr(::Val{:cond}, args::V)
    # TODO
end

# TODO: try/catch/finally, let, defun, defmacro.

# Equivalent to [h | t] = list.
peel(args::V) = args[1], args[2:end]

end
