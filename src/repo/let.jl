# A let binding.
# Example: {:let {{x 1} {y 2}} {+ x y}}
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
