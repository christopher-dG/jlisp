using JLisp
using Test

macro test_sexp(ex, expected)
    quote
        let ex = JLisp.sexp($(QuoteNode(ex)))
            @test ex == $expected
        end
    end
end

@testset "jlisp" begin
    @testset "S-expression parsing" begin
        @test_sexp {max 1 2} Expr(:call, :max, 1, 2)
        @test_sexp {+ 1 2} Expr(:call, :+, 1, 2)
        @test_sexp {❜> 1 2} Expr(:call, :>, 1, 2)
        @test_sexp {:def x 1} Expr(:(=), :x, 1)
        @test_sexp {:if true 1 2} Expr(:if, :true, 1, 2)
        @test_sexp {:for {x xs} x} Expr(:for, Expr(:(=), :x, :xs), :x)
        @test_sexp {:while true fun} Expr(:while, :true, :fun)
    end

    # @testset "String parsing" begin
    #     code = """
    #         {:def x 1}
    #         {:while {❜< x 10}
    #             {def x {+ x 1}}}
    #         {:if {❜!= x 10}
    #             {error "Something is broken!"}}
    #         """
    #     ex = JLisp.parse(code)
    #     @test ex == Expr(:block, Any[
    #         Expr(:(=), :x, 1),
    #         Expr(
    #             :while,
    #             Expr(:call, :<, :x, 10),
    #             Expr(:(=), :x, Expr(:call, :+, :x, 1)),
    #         ),
    #         Expr(
    #             :if,
    #             Expr(:call, :!=, :x, 10),
    #             Expr(:call, :error, "Something is broken!"),
    #         ),
    #     ]
    # end
end
