baremodule JLisp
export @jlisp

using Base

"""
    @jlisp {your {sexp here}}

Parse and execute some jlisp code.
"""
macro jlisp(ex)
    esc(sexp(ex; flatten=true))
end

# TODO: using, import. Relative imports are really tricky.

Base.include(JLisp,"io.jl")
Base.include(JLisp,"syntaxerror.jl")
Base.include(JLisp,"utils.jl")
Base.include(JLisp,"sexp.jl")

Base.include(JLisp,"repo.jl")


end
