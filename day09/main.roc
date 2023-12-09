app "day09"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        "input.txt" as input : Str,
    ]
    provides [main] to pf

# exampleInput = Str.trimStart
#     """
#     0 3 6 9 12 15
#     1 3 6 10 15 21
#     10 13 16 21 30 45
#     """

parse = \in -> in
    |> Str.split "\n"
    |> List.keepOks (\line -> Str.split line " " |> List.mapTry Str.toI128)

differences = \seq ->
    first = List.first seq |> Result.withDefault 0
    rest = List.dropFirst seq 1
    { diffs } = List.walk rest { diffs: [], prev: first } (\{ diffs: d, prev }, x ->
        {
            diffs: List.append d (x - prev),
            prev: x
        }
    )
    diffs

extrapolateForwards = \seq ->
    if List.isEmpty seq || List.all seq (\e -> e == 0) then
        0
    else
        last = List.last seq |> Result.withDefault 0
        last + (differences seq |> extrapolateForwards)

extrapolateBackwards = \seq ->
    if List.isEmpty seq || List.all seq (\e -> e == 0) then
        0
    else
        first = List.first seq |> Result.withDefault 0
        first - (differences seq |> extrapolateBackwards)

main = 
    sequences = parse input
    part1 = List.map sequences extrapolateForwards |> List.sum
    part2 = List.map sequences extrapolateBackwards |> List.sum

    Stdout.line "\(Num.toStr part1)\n\(Num.toStr part2)"        
        