app "day08"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        "input.txt" as input : Str,
    ]
    provides [main] to pf

exampleInput = Str.trimStart
    """
    LLR

    AAA = (BBB, BBB)
    BBB = (AAA, ZZZ)
    ZZZ = (ZZZ, ZZZ)
    """

parseNode : Str -> Result (Str, (Str, Str)) _
parseNode = \line ->
    { before: start, after: next } <- Str.splitFirst line " = " |> Result.try
    { before: leftStr, after: rightStr } <- Str.splitFirst next ", " |> Result.try
    left <- Str.toUtf8 leftStr |> List.dropFirst 1 |> Str.fromUtf8 |> Result.try
    right <- Str.toUtf8 rightStr |> List.dropLast 1 |> Str.fromUtf8 |> Result.try

    Ok (start, (left, right))

parse = \in ->
    map =
        { before: instStr, after: nodesStr } <- Str.splitFirst in "\n" |> Result.try
        nodes =
            Str.split nodesStr "\n"
            |> List.keepOks parseNode
            |> List.walk (Dict.empty {}) (\nds, (start, next) -> Dict.insert nds start next)

        Ok { instructions: Str.toUtf8 instStr, nodes }

    when map is
        Ok m -> m
        _ -> crash "Failed to parse"

walk = \map, steps, node, target ->
    if target node then
        steps
    else
        i = steps % List.len map.instructions
        instruction = List.get map.instructions i
        nextNode = Dict.get map.nodes node
        when (instruction, nextNode) is
            (Ok 'L', Ok (l, _)) -> walk map (steps + 1) l target
            (Ok 'R', Ok (_, r)) -> walk map (steps + 1) r target
            _ ->
                # dbg
                #     (instruction, nextNode)
                crash "Unknown instruction"

part1 = \in ->
    map = parse in
    walk map 0 "AAA" (\node -> node == "ZZZ")

gcd = \a, b ->
    if a == 0 then
        b
    else if b == 0 then
        a
    else if a > b then
        gcd b (a % b)
    else
        gcd a (b % a)

expect gcd 18 48 == 6
expect gcd 48 18 == 6

lcm = \a, b -> Num.abs a * (Num.abs b // gcd a b)

expect lcm 4 6 == 12

part2 = \in ->
    map = parse in
    startNodes = Dict.keys map.nodes |> List.keepIf (\node -> Str.endsWith node "A")
    dbg
        startNodes

    # ["AAA", "RLA", "QLA", "QFA", "RXA", "JSA"]
    # 18157 14363 16531 12737 19783 19241
    steps = List.map startNodes (\node -> walk map 0 node (\n -> Str.endsWith n "Z"))
    dbg
        steps

    List.walk steps 1 (\l, s -> lcm l s)

main =
    _ <- part1 input |> Num.toStr |> Stdout.line |> Task.await
    _ <- part2 input |> Num.toStr |> Stdout.line |> Task.await
    Task.ok {}
