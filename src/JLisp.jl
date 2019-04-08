module JLisp

export @jlisp

"""
    @jlisp {your {sexp here}}

Parse and execute some jlisp code.
"""
macro jlisp(ex)
    esc(sexp(ex; flatten=true))
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
        push!(block.args, sexp(ex; flatten=true))
    end
    block
end

struct SyntaxError <: Exception
    msg::String
end

Base.showerror(io::IO, e::SyntaxError) = print(io, "Syntax error: ", e.msg)

macro se_str(s)
    :(throw(SyntaxError($s)))
end

# A binding for a keyword expression: {varname varval}.
isbinding(x) = false
isbinding(ex::Expr) = ex.head === :call && length(ex.args) == 2 && first(ex.args) isa Symbol

# An operator tied to assignment: +=.
isopassign(x) = false
function isopassign(s::Symbol)
    str = string(s)
    length(str) == 2 && endswith(str, '=') &&
        !isdefined(Base, s) && isdefined(Base, Symbol(first(str)))
end

# Converts any :bracescat expressions into a single :row expression.
flatten!(x) = x
function flatten!(ex::Expr)
    if ex.head === :bracescat
        ex.head = :row
        ex.args = mapreduce(vcat, ex.args) do arg
            arg isa Expr && arg.head === :row ? arg.args : [arg]
        end
    end
    foreach(flatten!, ex.args)
    ex
end

# Creates a struct definition.
function defstruct(mut::Bool, head, args...)
    block = Expr(:block)
    foreach(args) do arg
        if arg isa Expr && arg.head === :row && length(arg.args) == 2 &&
            first(arg.args) isa Symbol
            # Typed argument.
            push!(block.args, Expr(:(::), first(arg.args), sexp(last(arg.args))))
        else
            push!(block.args, arg)
        end
    end
    Expr(:struct, mut, head, block)
end

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

# This catches "escaped" operators that would otherwise cause parse errors.
# Example: {:+ 1 2}.
function kwexpr(::Val{F}, args...) where F
    if isopassign(F)
        Expr(F, map(sexp, args)...)
    elseif isletter(first(string(F)))
        se"Only operators can be escaped with :"
    else
        sexp(Val(:row), F, args...)
    end
end

# An assignment, i.e. var = val.
# Example: {:def x 1}.
kwexpr(::Val{:def}, args...) = se"Wrong number of arguments to :def"
kwexpr(::Val{:def}, arg1, arg2) = se"First argument to :def must be an identifier"
kwexpr(::Val{:def}, arg1::Symbol, arg2) = Expr(:(=), arg1, sexp(arg2))

# A const assignment.
# Example: {:const x 1}.
kwexpr(::Val{:const}, args...) = se"Wrong number of arguments to :const"
kwexpr(::Val{:const}, arg1, arg2) = Expr(:const, kwexpr(Val(:def), arg1, arg2))

# An if-else expression that does not contain any elseif.
# Example: {:if {fun args} {do this} {otherwise this}}.
kwexpr(::Val{:if}, args...) = se"Not enough arguments to :if"
kwexpr(::Val{:if}, arg1, arg2, arg3, arg4, args...) =
    se"Too many arguments to :if (use :cond for elseif)"
function kwexpr(::Val{:if}, arg1, arg2, arg3=nothing)
    ex = Expr(:if, sexp(arg1), sexp(arg2))
    arg3 === nothing || push!(ex.args, sexp(arg3))
    ex
end

# A for loop.
# Example: {:for {i 1:10} {print i}}.
kwexpr(::Val{:for}, args...) = se"Invalid :for iterator"
kwexpr(::Val{:for}, arg1::Expr, args...) = se"Wrong number of arguments to :for"
function kwexpr(::Val{:for}, arg1::Expr, arg2=nothing)
    it = sexp(arg1)
    isbinding(it) || se"Invalid :for iterator"
    ex = Expr(:for, Expr(:(=), first(it.args), sexp(last(it.args))))
    arg2 === nothing || push!(ex.args, sexp(arg2))
   ex
end

# A while loop.
# Example: {:while {pred} body}.
kwexpr(::Val{:while}, args...) = se"Wrong number of arguments to :while"
function kwexpr(::Val{:while}, arg1, arg2=nothing)
    ex = Expr(:while, sexp(arg1))
    arg2 === nothing || push!(ex.args, sexp(arg2))
    ex
end

# A let binding.
# Example: {:let {{x 1} {y 2}} {+ x y}}.
kwexpr(::Val{:let}, arg1, arg2) = se"Invalid :let binding"
kwexpr(::Val{:let}, args...) = Expr(:let, Expr(:block), Expr(:block, map(sexp, args)...))
function kwexpr(::Val{:let}, arg1::Expr, args...)
    if arg1.head === :braces
        # {:let {varname varval} ...}
        length(arg1.args) == 1 || se"Invalid :let binding"
        arg1 = Expr(:row, arg1.args...)
    elseif length(arg1.args) == 2 && first(arg1.args) isa Symbol
        # {:let {{varname varval}} ...}
        arg1 = Expr(:row, arg1)
    end
    arg1.head === :row || se"Invalid :let binding"
    exs = map(sexp, arg1.args)
    all(isbinding, exs) || se"Invalid :let binding"
    binds = map(ex -> Expr(:(=), first(ex.args), sexp(last(ex.args))), exs)
    Expr(:let, Expr(:block, binds...), Expr(:block, map(sexp, args)...))
end

# A return statement.
# Example: {:return 1}.
kwexpr(::Val{:return}) = Expr(:return, nothing)
kwexpr(::Val{:return}, arg1) = Expr(:return, sexp(arg1))
kwexpr(::Val{:return}, args...) = se"Invalid return statement"

# A struct definition.
# Example: {:struct Foo field {field2 Type} {:function Foo {} {new 0 0}}}.
kwexpr(::Val{:struct}, arg1, args...) = defstruct(false, arg1, args...)

# A mutable struct definition (same syntax as :struct).
kwexpr(::Val{:mutable}, arg1, args...) = defstruct(true, arg1, args...)

# An abstract type definition.
# Example: {:abstract Foo{T} <: Super}
kwexpr(::Val{:abstract}, args...) = Expr(:abstract, map(sexp, args)...)

# A primitive type definition.
# Example: {:primitive Foo <: Super 32}
kwexpr(::Val{:primitive}, args...) = Expr(:primitive, map(sexp, args)...)

# A quoted block.
# Example: {:quote {fun} {other arg} result}.
kwexpr(::Val{:quote}, args...) = Expr(:quote, Expr(:block, map(sexp, args)...))

# A block start.
# Example: {:begin {fun} {other arg} result}.
kwexpr(::Val{:begin}, args...) = Expr(:block, map(sexp, args)...)

# A module definition.
# Example: {:module Foo {body} {morebody}}
kwexpr(::Val{:module}, args...) = se"Invalid module name"
kwexpr(::Val{:module}, arg1::Symbol, args...) =
    Expr(:module, true, arg1, Expr(:block, map(sexp, args)...))

# A bare module definition (same syntax as :module).
kwexpr(::Val{:baremodule}, args...) = se"Invalid module name"
kwexpr(::Val{:baremodule}, arg1::Symbol, args...) =
    Expr(:module, false, arg1, Expr(:block, map(sexp, args)...))

# A function definition.
# Example: {:function foo {arg1 arg2::Type arg3::Type=default :kw kw1}}
function kwexpr(::Val{:function}, arg1, arg2::Expr, args...)
    # TODO: arg2 arguments must be inserted into arg1 :call.
end

# A multi-branch if statement, i.e. elseif.
# Example: {:cond {{pred1 x} body1} {{pred2 x} body2} {true body3}}.
# Note that no else is inserted.
function kwexpr(::Val{:cond}, args...)
    # TODO
end

# TODO: try, function, macro, using, import.

end
