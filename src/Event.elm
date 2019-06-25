module Event exposing (..)

import Clock exposing (Time)
import Dict exposing (Dict)

import Time
import Clock exposing (RawTime, Time, fromRawParts, getHours, getMinutes)
import Html exposing (Html, a, div, text, h2, h3, h4, input)
import Html.Attributes exposing (class, id)
import Markdown
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


-- View
viewEvent : Event -> Html msg
viewEvent e =
    div [ class "event"
        , class (categoryToSymbol e.category) ]
        [ div [ class "row" ]
              [ div [ class "title" ] [ text e.title ]
              , text <| categoryToEmoji e.category
              , text <| if e.kidFriendly then "🧸" else "" ]
        , div [ class "row" ]
              [ div [ class "time" ]
                    [ text <| if e.allDay then
                                  "All Day"
                              else
                                  (timeToString e.time) ++ "–"
                                      ++ (timeToString (Clock.fromPosix <|
                                                            Time.millisToPosix <|
                                                            (Clock.toMillis e.time)+e.duration*60000))
                    ]
              , div [ class "days" ] (List.map (\d ->
                                                    div [ class "day" ]
                                                    [ text <| dayToShortString d ])
                                          e.dates)]
        , Markdown.toHtml [ class "description" ] e.description
        , div [ class "row" ]
              [ div [ class "camp" ] [ text e.camp ]
              , div [ class "host" ] [ text e.host ]
              ]]

timeToString t = (getHours t |> String.fromInt |> String.pad 2 '0')
               ++ ":"
               ++ (getMinutes t |> String.fromInt |> String.pad 2 '0')

