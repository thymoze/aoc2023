app "day05"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        "input.txt" as input : Str,
    ]
    provides [main] to pf

exampleInput = Str.trimStart
    """
    seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4
    """

expect exampleInput |> Str.split "\n" |> part1 == Ok 35
expect exampleInput |> Str.split "\n" |> part2 == Ok 46

Range : { src : U64, dest : U64, len : U64 }
Map : List Range

parseSeeds = \line -> line
    |> Str.splitFirst ": "
    |> Result.map .after
    |> Result.map (\v -> Str.split v " ")
    |> Result.map (\nums -> List.keepOks nums Str.toU64)
    |> Result.withDefault []

parseRange : Str -> Range
parseRange = \line ->
    when line |> Str.split " " |> List.keepOks Str.toU64 is
        [dest, src, len] -> { src, dest, len }
        _ -> crash "Malformed range"

parseNextMap : List Str -> Result { map : Map, rest : List Str } _
parseNextMap = \lines ->
    mapIdx <- List.findFirstIndex lines (\line -> Str.contains line "map") |> Result.try
    nextMapIdx = lines |> List.dropFirst (mapIdx + 1) |> List.findFirstIndex (\line -> Str.contains line "map") |> Result.withDefault (List.len lines)

    ranges =
        List.sublist lines { start: mapIdx + 1, len: nextMapIdx - 1 }
        |> List.keepIf (\line -> !(Str.isEmpty line))
        |> List.map parseRange

    Ok { map: ranges, rest: List.dropFirst lines (nextMapIdx + 1) }

mapCategory : U64, Map -> U64
mapCategory = \category, map ->
    List.walkUntil
        map
        category
        (\cat, { src, dest, len } ->
            if cat >= src && cat <= (src + len) then
                Break (dest + (cat - src))
            else
                Continue cat
        )

walkMaps : List U64, List Str -> List U64
walkMaps = \categories, lines ->
    when parseNextMap lines is
        Ok { map, rest } -> categories |> List.map (\c -> mapCategory c map) |> walkMaps rest
        Err _ -> categories

part1 = \lines ->
    seeds <- List.first lines |> Result.map parseSeeds |> Result.try
    rest = List.dropFirst lines 1

    locations = walkMaps seeds rest

    List.min locations

parseSeedRange = \line -> line
    |> parseSeeds
    |> List.chunksOf 2
    |> List.map
        (\chunk ->
            when chunk is
                [start, len] -> { start, len }
                _ -> crash "Malformed seeds range"
        )

CategoryRange : { start : U64, len : U64 }

overlap : CategoryRange, Range -> { mapped : Result CategoryRange _, notMapped : List CategoryRange }
overlap = \cat, range ->
    catStart = cat.start
    catEnd = catStart + cat.len

    srcStart = range.src
    srcEnd = range.src + range.len

    if srcEnd <= catStart || srcStart >= catEnd then
        { mapped: Err OutsideBounds, notMapped: [cat] }
    else if srcStart <= catStart && srcEnd >= catEnd then
        {
            mapped: Ok { start: (catStart - srcStart) + range.dest, len: cat.len },
            notMapped: [],
        }
    else if srcStart > catStart && srcEnd >= catEnd then
        {
            mapped: Ok { start: range.dest, len: catEnd - srcStart },
            notMapped: if srcStart - catStart == 0 then
                []
            else
                [{ start: catStart, len: cat.len - (catEnd - srcStart) }],
        }
    else if srcStart <= catStart && srcEnd < catEnd then
        {
            mapped: Ok { start: (catStart - srcStart) + range.dest, len: srcEnd - catStart },
            notMapped: if catEnd - srcEnd == 0 then
                []
            else
                [{ start: srcEnd, len: catEnd - srcEnd }],
        }
    else if srcStart > catStart && srcEnd < catEnd then
        {
            mapped: Ok { start: range.dest, len: range.len },
            notMapped: [
                { start: catStart, len: srcStart - catStart },
                { start: srcEnd, len: catEnd - srcEnd },
            ],
        }
    else
        crash "unreachable"

mapCatRange : CategoryRange, Map -> List CategoryRange
mapCatRange = \cat, map ->
    when map is
        [range, .. as rest] ->
            { mapped, notMapped } = overlap cat range

            list =
                when rest is
                    [] -> notMapped
                    _ -> List.joinMap notMapped (\s -> mapCatRange s rest)
            when mapped is
                Ok m -> List.append list m
                Err _ -> list

        [] -> []

walkMaps2 : List CategoryRange, List Str -> List CategoryRange
walkMaps2 = \categories, lines ->
    when parseNextMap lines is
        Ok { map, rest } -> categories |> List.joinMap (\c -> mapCatRange c map) |> walkMaps2 rest
        Err _ -> categories

part2 = \lines ->
    seeds <- List.first lines |> Result.map parseSeedRange |> Result.try
    rest = List.dropFirst lines 1

    walkMaps2 seeds rest |> List.map .start |> List.min

main =
    lines =
        input
        |> Str.split "\n"

    _ <- part1 lines |> Result.withDefault 0 |> Num.toStr |> Stdout.line |> Task.await
    _ <- part2 lines |> Result.withDefault 0 |> Num.toStr |> Stdout.line |> Task.await

    Task.ok {}
