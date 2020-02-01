
"""
    JLisp.Base.include(file)

The same as regular `Base.include`, but works on jlisp files.
"""
Base.include(file) = Core.eval(JLisp, open(parse, file))

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
