module Event exposing (..)

import Clock exposing (Time)
import Dict exposing (Dict)
import Regex

type Category = Category String
type Day = Day String

type alias Camp = String

type alias Event =
    { id : String
    , title : String
    , host : String
    , camp : Camp
    , category : Category
    , description : String
    , kidFriendly : Bool
    , dates : List Day
    , time : Time
    , duration : Int
    , allDay : Bool
    }


categoryEnum : List Category
categoryEnum = [ Category "DJ/Music (might get separate presentation)"
               , Category "Workshop/Class"
               , Category "Care/Support"
               , Category "Fire"
               , Category "Food"
               , Category "Games"
               , Category "Parade"
               , Category "Party/Gathering"
               , Category "Performance"
               , Category "Ritual/Ceremony"
               ]
categoryEmojiEnum : Dict String String
categoryEmojiEnum = List.map2 Tuple.pair
                    (List.map categoryToString categoryEnum)
                    [ "🎶"
                    , "🎓"
                    , "🤗"
                    , "🔥"
                    , "🍔"
                    , "🕹️"
                    , "🥁"
                    , "🎉"
                    , "🤡"
                    , "😈"
                    ] |> Dict.fromList
categoryToString : Category -> String
categoryToString (Category c) = c


categoryToSymbol : Category -> String
categoryToSymbol (Category c) = String.toLower c |> Regex.replace (Regex.fromString "[^a-z]" |> Maybe.withDefault Regex.never) (\_ -> "")

categoryToEmoji : Category -> String
categoryToEmoji c = Dict.get (categoryToString c) categoryEmojiEnum |> Maybe.withDefault "👽"

dayToString (Day c) = c
dayToShortString (Day c) = String.left 3 c



