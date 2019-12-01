module Event exposing (..)

import Clock exposing (Time)
import Dict exposing (Dict)

import Time
import Clock exposing (RawTime, Time, fromRawParts, getHours, getMinutes)
import Html exposing (Html, a, div, text, h2, h3, h4, input)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Markdown
import Regex

import Set exposing (Set)

import Color exposing (Color)
import Palette.Generative
import Palette.X11
import Murmur3

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

type Msg = Fav String
         | UnFav String

type alias CategoryMeta = { emoji : String, color : Color }

categoryEnum : List Category
categoryEnum = [ Category "DJ/Music"
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

categoryEmojiEnum : List String
categoryEmojiEnum =
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
    ]

categoryToString : Category -> String
categoryToString (Category c) = c

categoryToSymbol : Category -> String
categoryToSymbol (Category c) = String.toLower c |> Regex.replace (Regex.fromString "[^a-z]" |> Maybe.withDefault Regex.never) (\_ -> "")

dayToString (Day c) = c
dayToShortString (Day c) = String.left 3 c

colorWheel startcolor l i =
    Color.rotateHue ((toFloat i) * 360 / (toFloat l)) startcolor

categoryColor i _ =
    let
        length = List.length categoryEnum
    in
        colorWheel (Color.fromRGB (167, 255, 145)) length i

categoryMeta : Dict String CategoryMeta
categoryMeta = -- TODO messy
    List.map2 Tuple.pair (List.map categoryToString categoryEnum)
        (List.map2 (\e -> \c -> { emoji = e, color = c}) categoryEmojiEnum (List.indexedMap categoryColor categoryEnum) )
        |> Dict.fromList

categoryGetMeta : Category -> CategoryMeta
categoryGetMeta (Category c) = Dict.get c categoryMeta
                             |> Maybe.withDefault { emoji = "💩", color = Palette.X11.red }

categoryToEmoji : Category -> String
categoryToEmoji c = categoryGetMeta c |> .emoji

categoryToColor : Category -> Color
categoryToColor c = categoryGetMeta c |> .color

campToColor : String -> Color
campToColor s = Murmur3.hashString 123 s |> colorWheel (Color.fromRGB (167, 255, 200)) 1280


-- View
viewEvent : Set String -> Event -> Html Msg
viewEvent favs e = -- TODO messy
    div [ class "event"
        , style "background" <| "linear-gradient(180deg, " ++ Color.toRGBString(categoryToColor e.category) ++ " 40%, " ++
                                Color.toRGBString(campToColor e.camp)
                                ++ " 99%)"
        , style "color" <| Color.toRGBString <| Color.highContrast <| categoryToColor e.category
        ]
        --, class (categoryToSymbol e.category) ]
        [ div [ class "content" ] [ div [ class "row" ]
              [ div [ class "title" ] [ text e.title ]
              , text <| categoryToEmoji e.category
              , text <| if e.kidFriendly then "🧸" else "" ]
        , div [ class "row" ]
              [ div [ class "time" ]
                    [ text <| if e.allDay then
                                  "All Day"
                              else
                                  (timeToString e.time) ++ "–"
                                      ++ (timeToString <| eventEndTime e)
                    ]
              , div [ class "days" ] (List.map (\d ->
                                                    div [ class "day" ]
                                                    [ text <| dayToShortString d ])
                                          e.dates)]
        , Markdown.toHtml [ class "description" ] e.description
        , div [ class "row lastRow"
              , style "color" <| Color.toRGBString <| Color.highContrast <| campToColor e.camp
              ]
              [ div [ class "camp"
                    ] [ text e.camp ]
              , div [ class "host" ] [ text e.host ]
              , if Set.member e.id favs then
                    div [ class "favbtn"
                        , onClick (UnFav e.id) ] [ text "💖" ]
                else
                    div [ class "favbtn"
                        , onClick (Fav e.id) ] [ text "♡" ]
              ]]
        ]

eventEndTime : Event -> Time
eventEndTime e = Clock.fromPosix <| Time.millisToPosix <| (Clock.toMillis e.time)+e.duration*60000

eventCsv : Event -> String
eventCsv e = List.map (\d -> String.join ","
                           [ csvEscape e.title
                           , categoryToString e.category
                           , if e.kidFriendly then "kid friendly" else "kid unfriendly"
                           , if e.allDay then "all day" else "not all day"
                           , timeToString e.time
                           , eventEndTime e |> timeToString
                           , csvEscape e.camp
                           , csvEscape e.host
                           , csvEscape e.description
                           , dayToString d
                           , List.map (\f -> dayToString f) e.dates |> String.join "," |> csvEscape
                           ]) e.dates
           |> String.join "\n"

csvEscape : String -> String
csvEscape s = "\"" ++ (String.replace "\n" " " s |> String.replace "\"" "\"\"") ++ "\""

timeToString t = (getHours t |> String.fromInt |> String.pad 2 '0')
               ++ ":"
               ++ (getMinutes t |> String.fromInt |> String.pad 2 '0')

