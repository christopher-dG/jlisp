struct SyntaxError <: Exception
    msg::String
end

Base.showerror(io::IO, e::SyntaxError) = print(io, "Syntax error: ", e.msg)

macro se_str(s)
    :(throw(SyntaxError($s)))
end
