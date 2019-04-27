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
        @test_sexp {:for {x xs} x} Expr(:for, Expr(:block, Expr(:(=), :x, :xs)), :x)
        @test_sexp {:for {{x xs}} x} Expr(:for, Expr(:block, Expr(:(=), :x, :xs)), :x)
        @test_sexp {:while true fun} Expr(:while, :true, :fun)
        @test_sexp {:module Foo 1} Expr(:module, true, :Foo, Expr(:block, 1))
        @test_sexp {:baremodule Foo 1} Expr(:module, false, :Foo, Expr(:block, 1))
        @test_sexp {:quote 1 2 3} Expr(:quote, Expr(:block, 1, 2, 3))
        @test_sexp {:begin 1 2 3} Expr(:block, 1, 2, 3)
        @test_sexp {:abstract Foo} Expr(:abstract, :Foo)
        @test_sexp {:abstract Foo <: Real} Expr(:abstract, Expr(:<:, :Foo, :Real))
        @test_sexp {:primitive Foo 8} Expr(:primitive, :Foo, 8)
        @test_sexp {:primitive Foo <: Real 8} Expr(:primitive, Expr(:<:, :Foo, :Real), 8)
        @test_sexp {:mutable Foo} Expr(:struct, true, :Foo, Expr(:block))
        @test_sexp {:return} Expr(:return, nothing)
        @test_sexp {:return 1} Expr(:return, 1)
        @test_sexp {:|| foo bar} Expr(:||, :foo, :bar)

        @test_sexp {:function foo {x :kw y=1 z::Bool} x} Expr(
            :function,
            Expr(
                :call,
                :foo,
                Expr(:parameters, Expr(:kw, :y, 1), Expr(:(::), :z, :Bool)),
                :x,
            ),
            Expr(:block, :x),
        )

        @test_sexp {:macro foo {x} x} Expr(
            :macro,
            Expr(:call, :foo, :x),
            Expr(:block, :x),
        )

        @test_sexp {:struct Foo a b::Int} Expr(
            :struct,
            false,
            :Foo,
            Expr(:block, :a, Expr(:(::), :b, :Int)),
        )

        @test_sexp {:let {{x 1} {y 2}} 1} Expr(
            :let,
            Expr(:block, Expr(:(=), :x, 1), Expr(:(=), :y, 2)),
            Expr(:block, 1),
        )

        @test_sexp {:cond {foo bar} {{foo bar} baz}} Expr(
            :if,
            :foo,
            Expr(:block, :bar),
            Expr(:elseif, Expr(:call, :foo, :bar), Expr(:block, :baz)),
        )

        @test_sexp {:try {foo} {:catch ex {rethrow}} {:finally {bar}}} Expr(
            :try,
            Expr(:block, Expr(:call, :foo)),
            :ex,
            Expr(:block, Expr(:call, :rethrow)),
            Expr(:block, Expr(:call, :bar)),
        )

        @test_sexp {:try {foo} {:catch {rethrow}} {:finally {bar}}} Expr(
            :try,
            Expr(:block, Expr(:call, :foo)),
            false,
            Expr(:block, Expr(:call, :rethrow)),
            Expr(:block, Expr(:call, :bar)),
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
