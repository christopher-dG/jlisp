# A while loop.
# Example: {:while {pred} {println "hi!"}}
kwexpr(::Val{:while}) = se"Wrong number of arguments to :while"
function kwexpr(::Val{:while}, arg1, args...)
    ex = Expr(:while, sexp(arg1))
    isempty(args) || append!(ex.args, map(sexp, args))
    ex
end
