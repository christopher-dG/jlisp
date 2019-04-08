using JLisp
using Test

macro test_sexp(ex, expected)
    quote
        let ex = JLisp.sexp($(QuoteNode(ex)); flatten=true)
            @test ex == $expected
        end
    end
end

@testset "jlisp" begin
    @testset "S-expression parsing" begin
        @test_sexp {max 1 2} Expr(:call, :max, 1, 2)
        @test_sexp {+ 1 2} Expr(:call, :+, 1, 2)
        @test_sexp {:> 1 2} Expr(:call, :>, 1, 2)
        @test_sexp {:def x 1} Expr(:(=), :x, 1)
        @test_sexp {:const x 1} Expr(:const, Expr(:(=), :x, 1))
        @test_sexp {:if true 1 2} Expr(:if, :true, 1, 2)
        @test_sexp {:for {x xs} x} Expr(:for, Expr(:(=), :x, :xs), :x)
        @test_sexp {:while true fun} Expr(:while, :true, :fun)
        @test_sexp {:let {{x 1} {y 2}} {+ x y}} Expr(
            :let,
            Expr(:block, Expr(:(=), :x, 1), Expr(:(=), :y, 2)),
            Expr(:block, Expr(:call, :+, :x, :y)),
        )

        ex = JLisp.sexp(:({@show {max 1 2}}); flatten=true)
        @test ex.head === :macrocall && first(ex.args) === Symbol("@show")
        @test last(ex.args) == Expr(:call, :max, 1, 2)
    end

    @testset "String parsing" begin
        code = """
            {:let {x 0}
                {:while {:< x 10}
                    {:+= x 1}}
                {:if {:!= x 10}
                    {error "Something is broken!"}}}
            """
        ex = JLisp.parse(code)
        expected = Expr(
            :block,
            Expr(
                :let,
                Expr(:block, Expr(:(=), :x, 0)),
                Expr(
                    :block,
                    Expr(
                        :while,
                        Expr(:call, :<, :x, 10),
                        Expr(:+=, :x, 1)
                    ),
                    Expr(
                        :if,
                        Expr(:call, :!=, :x, 10),
                        Expr(:call, :error, "Something is broken!"),
                    ),
                ),
            ),
        )
        @test ex == expected
    end
end
