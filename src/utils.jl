
# A binding for a keyword expression: {varname varval}.
isbinding(x) = false
isbinding(ex::Expr) = ex.head === :call && length(ex.args) == 2 && first(ex.args) isa Symbol

# An operator tied to assignment: +=.
isopassign(x) = false
function isopassign(s::Symbol)
    str = string(s)
    length(str) == 2 && last(str) == '=' &&
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

# This catches some special operators that have their own expression heads.
for F in map(QuoteNode, (:||, :&&))
    @eval kwexpr(::Val{$F}, args...) = Expr($F, map(sexp, args)...)
end

# This catches other "escaped" operators that would otherwise cause parse errors.
# Example: {:+= 1 2}
function kwexpr(::Val{F}, args...) where F
    if isopassign(F)
        Expr(F, map(sexp, args)...)
    elseif isletter(first(string(F)))
        se"Only operators can be escaped with :"
    else
        sexp(Val(:row), F, args...)
    end
end
