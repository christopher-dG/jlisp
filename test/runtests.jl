using JLisp: parse
using Test

@testset "JLisp" begin
    ex = parse("{max 1 2}")
    @test ex == [:(max(1, 2))]
    ex = parse("{+ 1 2}")
    @test ex == [:(1 + 2)]
    ex = parse("{}")
end
