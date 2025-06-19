module RTTL.Tone exposing
    ( Duration(..)
    , DurationLength(..)
    , Tone(..)
    , encode
    )

import Json.Encode as Encode
import RTTL.Pitch as Pitch exposing (Pitch)


type DurationLength
    = Whole
    | Half
    | Quarter
    | Eighth
    | Sixteenth
    | ThirtySecond


type Duration
    = Normal DurationLength
    | Dotted DurationLength


type Tone
    = Tone Pitch Duration
    | Pause Duration


encode : Int -> Tone -> Encode.Value
encode tempo tone =
    case tone of
        Tone pitch duration ->
            Encode.object
                [ ( "frequency", Pitch.encode pitch )
                , ( "duration", encodeDuration tempo duration )
                ]

        Pause duration ->
            Encode.object
                [ ( "frequency", Encode.float 0 )
                , ( "duration", encodeDuration tempo duration )
                ]


encodeDuration : Int -> Duration -> Encode.Value
encodeDuration tempo duration =
    let
        -- What's this calculation? Imagine we are at a 100 BPM.
        -- We want 100 beats every sixty seconds, so each beat
        -- would have to last 60/100 seconds.
        base =
            60 / toFloat tempo

        -- A whole note lasts the whole bar. Since there is no time
        -- signature in the composer format, we assume it's always 4/4
        factor length =
            case length of
                Whole ->
                    4

                Half ->
                    2

                Quarter ->
                    1

                Eighth ->
                    0.5

                Sixteenth ->
                    0.25

                ThirtySecond ->
                    0.125

        result =
            case duration of
                Normal dLength ->
                    base * factor dLength

                Dotted dLength ->
                    base * 1.5 * factor dLength
    in
    Encode.float result
