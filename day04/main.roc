app "day04"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        "input.txt" as input : Str,
    ]
    provides [main] to pf

# example = Str.trimStart
#     """
#     Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
#     Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
#     Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
#     Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
#     Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
#     Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
#     """

Card : {
    id : U32,
    winning : Set Str,
    have : Set Str,
    intersectSize : Nat
}

parseCard : Str -> Result Card _
parseCard = \card -> 
    { before: cardId, after: numbers } <- Str.splitFirst card ": " |> Result.try
    { before: winningStr, after: haveStr } <- Str.splitFirst numbers " | " |> Result.try

    id <- Str.splitLast cardId " " |> Result.try (\c -> c.after |> Str.toU32) |> Result.try

    toSet = (\str -> str
        |> Str.split " " 
        |> List.keepIf (\s -> !(s |> Str.trim |> Str.isEmpty)) 
        |> Set.fromList)

    winning = toSet winningStr
    have = toSet haveStr
    intersect = Set.intersection winning have
    intersectSize = Set.len intersect

    Ok { id, winning, have, intersectSize }

part1 = \lines -> lines 
    |> List.keepOks (\l ->
        { intersectSize } <- parseCard l |> Result.try

        if intersectSize > 0 then
            points = Num.shiftLeftBy 1 (Num.toU8 intersectSize - 1)
            Ok points
        else
            Err NoWinners
    )
    |> List.sum

part2 = \lines ->  
    cards = lines |> List.keepOks parseCard
    
    List.walk cards (Dict.empty {}) (\state, {id, intersectSize} ->
        cardCount = Dict.get state id |> Result.map (\v -> v + 1) |> Result.withDefault 1
        
        List.range { start: After id, end: At (id + Num.toU32 intersectSize)}
        |> List.walk state (\d, i ->
            Dict.update d i (\existing -> when existing is
                Missing -> Present cardCount
                Present val -> Present (val + cardCount)
            )
        )
        |> Dict.insert id cardCount
    )
    |> Dict.values
    |> List.sum

main =
    lines = input
        |> Str.split "\n"

    _ <- lines |> part1 |> Num.toStr |> Stdout.line |> Task.await
    _ <- lines |> part2 |> Num.toStr |> Stdout.line |> Task.await

    Task.ok {}