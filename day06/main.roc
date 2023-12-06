app "day06"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        "input.txt" as input : Str,
    ]
    provides [main] to pf

Race : { time : U64, distance : U64 }

parseSingle : Str -> Race
parseSingle = \in ->
    nums =
        Str.replaceEach in " " ""
        |> Str.split "\n"
        |> List.keepOks
            (\l ->
                Str.splitFirst l ":" |> Result.map .after |> Result.try Str.toU64
            )

    when nums is
        [time, distance] ->
            { time, distance }

        _ -> crash "Malformed input"

parseMultiple : Str -> List Race
parseMultiple = \in ->
    nums =
        Str.split in "\n"
        |> List.map (\l -> Str.split l " " |> List.keepOks Str.toU64)

    when nums is
        [times, distances, ..] ->
            List.map2 times distances (\time, distance -> { time, distance })

        _ -> crash "Malformed input"

raceTime = \race ->
    List.range { start: At 0, end: At race.time }
    |> List.walkUntil
        0
        (\c, i ->
            if i * (race.time - i) > race.distance then
                Break ((race.time + 1) - (2 * c))
            else
                Continue (c + 1)
        )

part1 = \in ->
    races = parseMultiple in
    List.walk
        races
        1
        (\total, race ->
            count = raceTime race
            total * count
        )

part2 = \in ->
    race = parseSingle in
    raceTime race

main =
    _ <- part1 input |> Num.toStr |> Stdout.line |> Task.await
    _ <- part2 input |> Num.toStr |> Stdout.line |> Task.await

    Task.ok {}
