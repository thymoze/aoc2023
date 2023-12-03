app "day03"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        "input.txt" as input : Str,
    ]
    provides [main] to pf

# exampleInput = Str.trimStart
#     """
#     467..114..
#     ...*......
#     ..35..633.
#     ......#...
#     617*......
#     .....+.58.
#     ..592.....
#     ......755.
#     ...$.*....
#     .664.598..
#     """

# debug = \v ->
#     dbg v
#     v

findNumber : List U8 -> Result { num: U32, begin: Nat, end: Nat } _
findNumber = \line ->
    begin <- List.findFirstIndex line (\v -> Str.fromUtf8 [v] |> Result.try Str.toU32 |> Result.isOk) |> Result.try

    rest = List.takeLast line ((List.len line) - begin)
    end <- List.findFirstIndex rest (\v -> Str.fromUtf8 [v] |> Result.try Str.toU32 |> Result.isErr) |> Result.try

    numStr = List.sublist line { start: begin, len: end }

    num = Str.fromUtf8 numStr |> Result.try Str.toU32

    Result.map num (\n -> { num: n, begin, end: begin + end })

findAllNumbers : List U8 -> List _
findAllNumbers = \line ->
    when findNumber line is
        Ok x -> List.takeLast line ((List.len line) - x.end) 
                |> findAllNumbers
                |> List.map (\v -> { v & begin: v.begin + x.end, end: v.end + x.end})
                |> List.prepend x
        _ -> []

find : List U8, (U8 -> Bool) -> List Nat
find = \chars, f -> 
    when List.findFirstIndex chars f is
        Ok idx -> List.takeLast chars ((List.len chars) - (idx + 1)) 
                    |> find f
                    |> List.map (\v -> v + idx + 1) 
                    |> List.prepend idx
        _ -> []

isSymbol = \char ->
    !(List.contains ['.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] char)    

findSymbols : List U8 -> List Nat
findSymbols = \chars -> find chars isSymbol

findGears = \chars -> find chars (\char -> char == '*')

searchNeighbors = \lines, lineIdx, symbolIdx ->
    [
        (lines |> List.get (lineIdx - 1) |> Result.withDefault [] |> findAllNumbers),
        (lines |> List.get (lineIdx) |> Result.withDefault [] |> findAllNumbers),
        (lines |> List.get (lineIdx + 1) |> Result.withDefault [] |> findAllNumbers)
    ]
    |> List.join 
    |> List.keepIf (\{ begin, end } -> 
        List.range { start: At begin, end: Before end }
        |> List.any (\v -> List.contains [symbolIdx - 1, symbolIdx, symbolIdx + 1] v)
    )
    

pad = \lines -> 
    lineLength = List.first lines |> Result.map Str.countUtf8Bytes |> Result.withDefault 0
    lines
    |> List.prepend (Str.repeat "." lineLength)
    |> List.append (Str.repeat "." lineLength)
    |> List.map (\line -> ".\(line).")

part1 = \lines ->
    List.walkWithIndex lines [] (\state, line, i -> 
        when findSymbols line is
            [] -> state
            symbols  -> 
                List.concat state (
                    List.walk symbols [] (\list, s ->
                        List.concat list (searchNeighbors lines i s)
                    )
                )
    ) 
    |> List.map .num
    |> List.sum

part2 = \lines ->
    List.walkWithIndex lines [] (\state, line, i -> 
        when findGears line is
            [] -> state
            symbols -> 
                List.concat state (
                    List.walk symbols [] (\list, s ->
                        when searchNeighbors lines i s is
                            [a, b] -> List.append list (a.num * b.num)
                            _ -> list
                    )
                )
    )
    |> List.sum

main =
    lines = input
        |> Str.split "\n"
        |> pad
        |> List.map Str.toUtf8

    _ <- part1 lines |> Num.toStr |> Stdout.line |> Task.await
    _ <- part2 lines |> Num.toStr |> Stdout.line |> Task.await

    Task.ok {}