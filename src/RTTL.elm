-- Copied from https://github.com/Arkham/elm-rttl/blob/master/src/RTTL.elm

module RTTL exposing
    ( Ringtone(..)
    , parseComposer
    , encode
    )

{-| Parse ringtones written using RTTL and Nokia Composer

@docs Ringtone


# Parsing

@docs parseComposer


# Encoding

@docs encode

-}

import Json.Encode as Encode
import Parser as P exposing ((|.), (|=), Parser)
import RTTL.Note exposing (Note(..))
import RTTL.Pitch exposing (Pitch(..))
import RTTL.Tone as Tone exposing (Duration(..), DurationLength(..), Tone(..))


type alias BPM =
    Int


{-| A ringtone has a well-defined tempo in BPM (beats-per-minute) and is
composed by multiple tones, which can be either pitches or pauses.
-}
type Ringtone
    = Ringtone
        { tempo : BPM
        , tones : List Tone
        }


{-| Parse a ringtone expressed in the Nokia Composer format. It is a less
generic format compared to RTTL since you can only express 3 octaves.
Since the format does not specify a tempo, you will need to choose one.

Here's an example: 4c2 4d2 4e2 16- 8.#a3

-}
parseComposer : { tempo : Int } -> String -> Result (List P.DeadEnd) Ringtone
parseComposer { tempo } input =
    P.run composerParser input
        |> Result.map
            (\tones ->
                Ringtone { tempo = tempo, tones = tones }
            )


composerParser : Parser (List Tone)
composerParser =
    P.loop [] composerParserHelper


composerParserHelper : List Tone -> Parser (P.Step (List Tone) (List Tone))
composerParserHelper acc =
    P.oneOf
        [ P.succeed (\v -> P.Loop (v :: acc))
            |. P.spaces
            |= composerToneParser
            |. P.spaces
        , P.succeed (P.Done (List.reverse acc))
        ]


composerToneParser : Parser Tone
composerToneParser =
    P.oneOf
        [ P.backtrackable <|
            P.succeed (\duration pitch -> Tone pitch duration)
                |= durationParser
                |= pitchParser
        , P.succeed Pause
            |= durationParser
            |. P.symbol "-"
        ]


pitchParser : Parser Pitch
pitchParser =
    let
        octaveParser =
            P.oneOf
                [ P.succeed 5
                    |. P.symbol "1"
                , P.succeed 6
                    |. P.symbol "2"
                , P.succeed 7
                    |. P.symbol "3"
                , P.succeed 8
                    |. P.symbol "4"
                , P.succeed 9
                    |. P.symbol "5"
                , P.succeed 10
                    |. P.symbol "6"
                ]
    in
    P.succeed Pitch
        |= noteParser
        |= octaveParser


noteParser : Parser Note
noteParser =
    P.oneOf
        [ withSharp "a" ( A, Bb )
        , withSharp "b" ( B, C )
        , withSharp "c" ( C, Db )
        , withSharp "d" ( D, Eb )
        , withSharp "e" ( E, F )
        , withSharp "f" ( F, Gb )
        , withSharp "g" ( G, Ab )
        ]


withSharp : String -> ( Note, Note ) -> Parser Note
withSharp note ( current, sharped ) =
    P.backtrackable <|
        P.oneOf
            [ P.succeed sharped
                |. P.symbol "#"
                |. P.symbol note
            , P.succeed current
                |. P.symbol note
            ]


durationParser : Parser Duration
durationParser =
    let
        durationLengthParser =
            P.oneOf
                [ P.succeed ThirtySecond
                    |. P.symbol "32"
                , P.succeed Sixteenth
                    |. P.symbol "16"
                , P.succeed Eighth
                    |. P.symbol "8"
                , P.succeed Quarter
                    |. P.symbol "4"
                , P.succeed Half
                    |. P.symbol "2"
                , P.succeed Whole
                    |. P.symbol "1"
                ]
    in
    P.oneOf
        [ P.backtrackable
            (P.succeed Dotted
                |= durationLengthParser
                |. P.symbol "."
            )
        , P.succeed Normal
            |= durationLengthParser
        ]


{-| Encode ringtone as a list of { frequency, duration } objects
-}
encode : Ringtone -> Encode.Value
encode (Ringtone { tempo, tones }) =
    Encode.list (Tone.encode tempo) tones
