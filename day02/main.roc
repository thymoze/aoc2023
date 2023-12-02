app "day01"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        "input.txt" as input : Str,
    ]
    provides [main] to pf

Game : {
    id: U32,
    rounds: List Round
}

Round : {
    red: U32,
    green: U32,
    blue: U32
}

maxRound : Round
maxRound = { red: 12, green: 13, blue: 14 }

isLte : Round, Round -> Bool
isLte = \a, b ->
    a.red <= b.red && a.green <= b.green && a.blue <= b.blue

parseRound : Str -> Round
parseRound = \round ->
    dict = Str.split round ", " 
        |> List.walk (Dict.empty {}) (\d, s -> 
            ({ before: countStr, after: color } <- Str.splitFirst s " " |> Result.try
            count <- Str.toU32 countStr |> Result.map
            Dict.insert d color count)
            |> Result.withDefault d
        )
    {
        red: Dict.get dict "red" |> Result.withDefault 0,
        green: Dict.get dict "green" |> Result.withDefault 0,
        blue: Dict.get dict "blue" |> Result.withDefault 0,
    }

parseGame : Str -> Result Game _
parseGame = \line ->
    { before: game, after: rest } <- Str.splitFirst line ": " |> Result.try
    id <- Str.splitLast game " " |> Result.map .after |> Result.try Str.toU32 |> Result.try
    rounds = Str.split rest "; " |> List.map parseRound
    Ok { id, rounds }

debug = \v ->
    dbg v
    v

part1 = \games ->
    List.keepIf games (\game ->
        List.all game.rounds (\r -> isLte r maxRound)
    )
    |> List.map .id
    |> List.sum

part2 = \games ->
    List.map games (\game -> 
        minRound = List.walk game.rounds { red: 0, green: 0, blue: 0 } (\min, r ->
            {
                red: Num.max min.red r.red,
                green: Num.max min.green r.green,
                blue: Num.max min.blue r.blue,
            }
        )
        minRound.red * minRound.green * minRound.blue
    )
    |> List.sum

in = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
        Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
        Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
        """
expect
    games = Str.split in "\n"
        |> List.keepOks parseGame

    part1 games == 8

expect
    games = Str.split in "\n"
        |> List.keepOks parseGame

    part2 games == 2286

main =
    games = Str.split input "\n"
        |> List.keepOks parseGame
    
    {} <- part1 games
        |> Num.toStr
        |> Stdout.line
        |> Task.await

    {} <- part2 games
        |> Num.toStr
        |> Stdout.line
        |> Task.await

    Task.ok {}

    