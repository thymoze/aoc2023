app "day07"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        "input.txt" as input : Str,
    ]
    provides [main] to pf

exampleInput = Str.trimStart
    """
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483
    """

expect part1 exampleInput == 6440
expect part2 exampleInput == 5905

Card : U32 # [A, K, Q, J, T, N9, N8, N7, N6, N5, N4, N3, N2]
Type : [FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, Pair, HighCard]

typeValue = \type ->
    when type is
        HighCard -> 1
        Pair -> 2
        TwoPair -> 3
        ThreeOfAKind -> 4
        FullHouse -> 5
        FourOfAKind -> 6
        FiveOfAKind -> 7

HandType : {
    cards : List Card,
    type : Type,
}

Hand : { type : HandType, bid : U64 }

parseCard : Str, Bool -> Card
parseCard = \c, useJoker ->
    when c is
        "A" -> 14
        "K" -> 13
        "Q" -> 12
        "J" -> if useJoker then 1 else 11
        "T" -> 10
        "9" -> 9
        "8" -> 8
        "7" -> 7
        "6" -> 6
        "5" -> 5
        "4" -> 4
        "3" -> 3
        "2" -> 2
        _ -> crash "unknown card \(c)"

parseHandType = \h, useJoker ->
    cards = Str.graphemes h |> List.map (\x -> parseCard x useJoker)
    counts =
        List.walk
            cards
            (Dict.empty {})
            (\count, card ->
                Dict.update
                    count
                    card
                    (\existing ->
                        when existing is
                            Missing -> Present 1
                            Present v -> Present (v + 1)
                    )
            )

    jokers = Dict.get counts 1 |> Result.withDefault 0

    sorted =
        Dict.remove counts 1
        |> Dict.toList
        |> List.sortWith (\(_, v1), (_, v2) -> Num.compare v2 v1)
        |> List.update 0 (\(c, x) -> (c, x + jokers))

    type =
        when sorted is
            [] | [(_, 5)] -> FiveOfAKind
            [(_, 4), ..] -> FourOfAKind
            [(_, 3), (_, 2)] -> FullHouse
            [(_, 3), ..] -> ThreeOfAKind
            [(_, 2), (_, 2), ..] -> TwoPair
            [(_, 2), ..] -> Pair
            [(_, 1), ..] -> HighCard
            x ->
                dbg
                    x

                crash "Card logic?"

    { cards, type }

parseHand : Str, Bool -> Result Hand _
parseHand = \h, useJoker ->
    { before: typeStr, after: bidStr } <- Str.splitFirst h " " |> Result.try
    bid <- Str.toU64 bidStr |> Result.try
    type = parseHandType typeStr useJoker

    Ok { type, bid }

parseHands : Str, Bool -> List Hand
parseHands = \in, useJoker ->
    Str.split in "\n"
    |> List.keepOks (\l -> parseHand l useJoker)

compareCards : List Card, List Card -> [LT, EQ, GT]
compareCards = \a, b ->
    when (a, b) is
        ([x, .. as xs], [y, .. as ys]) ->
            when Num.compare x y is
                EQ -> compareCards xs ys
                comp -> comp

        _ -> EQ

compareType : HandType, HandType -> [LT, EQ, GT]
compareType = \a, b ->
    when Num.compare (typeValue a.type) (typeValue b.type) is
        EQ -> compareCards a.cards b.cards
        comp -> comp

solve = \in, useJoker ->
    hands = parseHands in useJoker |> List.sortWith (\a, b -> compareType a.type b.type)
    List.walkWithIndex hands 0 (\res, { bid }, i -> res + (bid * (Num.toU64 i + 1)))

part1 = \in -> solve in Bool.false

part2 = \in -> solve in Bool.true

main =
    _ <- part1 input |> Num.toStr |> Stdout.line |> Task.await
    _ <- part2 input |> Num.toStr |> Stdout.line |> Task.await

    Task.ok {}
