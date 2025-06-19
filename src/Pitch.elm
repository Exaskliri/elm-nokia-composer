module RTTL.Pitch exposing
    ( Pitch(..), Octave
    , build
    , next, transpose
    , toNote, encode
    )

{-| Represents a note in a certain octave

@docs Pitch, Octave


# Creating Pitches

@docs build


# Manipulating Pitches

@docs next, transpose


# Exporting

@docs toNote, encode

-}

import Json.Encode as Encode
import RTTL.Note as Note exposing (Note(..))


{-| The octave of our pitch.
-}
type alias Octave =
    Int


{-| A Pitch describes a note in a certain octave.
-}
type Pitch
    = Pitch Note Octave


{-| Creates a Pitch.
-}
build : Note -> Octave -> Pitch
build =
    Pitch


{-| Returns the next Pitch, which could be in another octave.
-}
next : Pitch -> Pitch
next (Pitch note octave) =
    let
        ( newNote, newOctave ) =
            case note of
                B ->
                    ( C, octave + 1 )

                other ->
                    ( Note.next other, octave )
    in
    Pitch newNote newOctave


{-| Returns the Pitch transposed by a number of semitones.
-}
transpose : Int -> Pitch -> Pitch
transpose count note =
    case count of
        0 ->
            note

        _ ->
            transpose (count - 1) (next note)


{-| Converts a Pitch to a Note.
-}
toNote : Pitch -> Note
toNote (Pitch note _) =
    note


{-| -}
encode : Pitch -> Encode.Value
encode (Pitch note octave) =
    let
        -- each seminote is separated by a factor of (2 ^ (1/12)),
        -- so we can just find the distance of the current pitch from
        -- the lowest pitch and solve: lowest * (2 ^ (distance / 12))
        noteDistance =
            case note of
                C ->
                    0

                Db ->
                    1

                D ->
                    2

                Eb ->
                    3

                E ->
                    4

                F ->
                    5

                Gb ->
                    6

                G ->
                    7

                Ab ->
                    8

                A ->
                    9

                Bb ->
                    10

                B ->
                    11

        -- the lowest pitch we can represent is (Pitch C 0), but since
        -- we usually define frequencies using A, we are going to shift
        -- the number we got by 9, which is the distance in semitones
        -- between C0 and A0.
        pitchDistance =
            octave * 12 + noteDistance - 9

        -- frequency of A0
        lowestA =
            27.5

        result =
            lowestA * (2 ^ (toFloat pitchDistance / 12))
    in
    Encode.float result
