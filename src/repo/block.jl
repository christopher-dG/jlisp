
# A quoted block.
# Example: {:quote {fun} {other arg} result}.
kwexpr(::Val{:quote}, args...) = Expr(:quote, Expr(:block, map(sexp, args)...))

# A block start.
# Example: {:begin {fun} {other arg} result}
kwexpr(::Val{:begin}, args...) = Expr(:block, map(sexp, args)...)
