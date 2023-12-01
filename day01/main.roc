app "day01"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        "input.txt" as input : Str,
    ]
    provides [main] to pf

part1 =
    Str.split input "\n"
    |> List.map Str.graphemes
    |> List.map (\l -> List.keepOks l Str.toU32)
    |> List.keepOks (\l -> 
        first <- Result.try (List.first l) 
        second <- Result.map (List.last l)
        first * 10 + second)
    |> List.sum

extractNumber = \txt, dir ->
    chars = Str.graphemes txt
    fn = (\state, elem ->
        s = if dir == Forward then (Str.concat state elem) else (Str.concat elem state)

        if Result.isOk (Str.toU32 elem) then
            Break elem
        else if Str.contains s "one" then
            Break "1"
        else if Str.contains s "two" then
            Break "2"
        else if Str.contains s "three" then
            Break "3"
        else if Str.contains s "four" then
            Break "4"
        else if Str.contains s "five" then
            Break "5"
        else if Str.contains s "six" then
            Break "6"
        else if Str.contains s "seven" then
            Break "7"
        else if Str.contains s "eight" then
            Break "8"
        else if Str.contains s "nine" then
            Break "9"
        else
            Continue s
    )

    (
        if dir == Forward then
            List.walkUntil chars "" fn
        else
            List.walkBackwardsUntil chars "" fn
    ) 
    |> Str.toU32

part2 =
    Str.split input "\n"
    |> List.keepOks (\line -> 
        first <- Result.try (extractNumber line Forward) 
        second <- Result.map (extractNumber line Backward)
        first * 10 + second)
    |> List.sum

main =
    {} <- Stdout.line (Num.toStr part1) |> Task.await
    {} <- Stdout.line (Num.toStr part2) |> Task.await
    Task.ok {}
    