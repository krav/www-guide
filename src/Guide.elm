module Guide exposing (..)

import Csv exposing (Csv, parse)
import Csv.Decode as CD exposing (Decoder, map, andMap, decode, field)
import Http
import List.Extra
import Clock exposing (RawTime, Time, fromRawParts, getHours, getMinutes)
import ElmTextSearch as ETS
import Html exposing (Html, a, div, text, h2, h3, h4, input)
import Html.Attributes exposing (class, id)
import Markdown

import Event exposing (Event, Category, Day)

type alias Events = List Event
type alias DaySchedule = List (Int, Events)
type alias Selection =
    { day : Day
    , sorting : (Event -> Event -> Order)
    , familyFriendly : Bool
    , category : Maybe Category
    --, location : Maybe String
    }
type alias Guide =
    { events : Events
    , index : ETS.Index Event
    , days : List Day
    }


new : String -> Result CD.Errors Guide
new s
    = parseCsv s |>
      Result.andThen (\e -> Ok
                          { events = e
                          , index = buildIndex e
                          , days = getDays e })

getDays : Events -> List Day
getDays _ = -- TODO
    --List.map (\e -> .day es |> List.Extra.uniqueBy (\(Event.Day s) -> s)
    [ (Event.Day "Monday 22."), (Event.Day "Tuesday 23."), (Event.Day "Wednesday 24."), (Event.Day "Thursday 25."), (Event.Day "Friday 26."), (Event.Day "Saturday 27."), (Event.Day "Sunday 28.")]

makeSchedule : Events -> DaySchedule
makeSchedule es = -- TODO all day events
    List.Extra.gatherWith
       (\e1 e2 -> getHours e1.time == getHours e2.time) es
   |> List.map (\(a, b) -> (getHours a.time, a::b))


filter : Selection -> Events -> Events
filter s e =
    filterByDay s.day e
    |> List.sortWith s.sorting

-- Sorting

sortByTime : Event -> Event -> Order
sortByTime e1 e2 = Clock.compare e1.time e2.time

-- sortByDay
-- must dupe

-- Filtering

filterByDay : Day -> Events -> Events
filterByDay d =
    List.filter (\e -> List.member d e.dates)

-- TODO by camp
-- TODO by type

-- Search

newIndex : ETS.Index Event
newIndex
    = ETS.new
      { ref = .id
      , fields =
          [ ( .title, 5.0 )
          , ( .description, 3.0 )
          , ( .camp, 1.0 )
          , ( .host, 1.0 ) ]
      , listFields = []
      }

buildIndex : Events -> ETS.Index Event
buildIndex es =
    ETS.addDocs es newIndex |> Tuple.first

search : String -> Guide -> Result String ( ETS.Index Event, List (String, Float))
search s g =
    ETS.search s g.index

-- Views

viewSchedule : DaySchedule -> Html msg
viewSchedule ds =
    div [] (List.map (\(h, es) ->
                  let
                      hour = (String.fromInt h |> String.pad 2 '0')++":00"
                  in
                  div [ ]
                      [ div [ class "hour" ] [ a [ id hour ] [ h2 [] [ text hour ] ]]
                      , viewEvents es ] )
                ds)

viewEvents : Events -> Html msg
viewEvents es =
    div [ class "events" ] (List.map viewEvent es)

viewEvent : Event -> Html msg -- TODO move
viewEvent e =
    div [ class "event"
        , class (Event.categoryToSymbol e.category) ]
        --, Html.Attributes.style "background-color" "lightblue" ]
        [ div [ class "row" ]
              [ div [ class "title" ] [ text e.title ]
              , text <| Event.categoryToEmoji e.category
              , text <| if e.kidFriendly then "🧸" else "" ]
        , div [ class "row" ]
              [ div [ class "time" ]
                    [ text <| (timeToString e.time) ++ "–" ++ "88:88"
                           -- duration incrementMinutes TODO
                    ]
              , div [ class "days" ] (List.map (\d ->
                                                    div [ class "day" ]
                                                    [ text <| Event.dayToShortString d ])
                                          e.dates)]
        , Markdown.toHtml [ class "description" ] e.description
        , div [ class "row" ]
              [ div [ class "camp" ] [ text e.camp ]
              , div [ class "host" ] [ text e.host ]
              ]]



timeToString t = (getHours t |> String.fromInt |> String.pad 2 '0')
               ++ ":"
               ++ (getMinutes t |> String.fromInt |> String.pad 2 '0')

-- Decode events csv

parseCsv : String -> Result CD.Errors Events
parseCsv s =
    Csv.parse s |> Result.withDefault ({headers = [], records =[]}) |> CD.decodeCsv decodeEvents

decodeEvents : Decoder (Event -> Event) Event
decodeEvents =
        map Event (field "Timestamp" Ok -- FIXME id
                  |> andMap (field "Title of Event" Ok)
                  |> andMap (field "Your preferred/playa name" Ok)
                  |> andMap (field "Location" Ok)
                  |> andMap (field "Type of Event" <|
                                 \c -> Ok (Event.Category c))
                  |> andMap (field "Event Description" Ok)
                  |> andMap (field "Is your event family friendly?" <|
                                 \s -> Ok (String.contains "Yes" s))
                  |> andMap (field "What day(s) is the event occurring" decodeDays)
                  |> andMap (field "At what time does the event start" decodeTime)
                  |> andMap (field "For how long does the event run" decodeDuration)
        )

decodeTime : String -> Result String Time
decodeTime s = String.split ":" s
             |> List.map String.toInt
             |> decodeTimeRaw
             |> fromRawParts
             |> Result.fromMaybe "Could not conquer time!"


decodeTimeRaw : List (Maybe Int) -> RawTime
decodeTimeRaw t =
    case t of
        [Just h, Just m, Just s] ->
            RawTime h m s 0
        _ ->
            RawTime 0 0 0 0

decodeDuration : String -> Result String Int
decodeDuration s =
    String.split ":" s
    |> List.map String.toInt
    |> (\l -> case l of
           ((Just h)::(Just m)::_) ->
               h*60 + m
           _ ->
               60)
    |> Ok

decodeDays : String -> Result String (List Day)
decodeDays s =
    String.split "," s
    |> List.map String.trim
    |> List.map Event.Day
    |> Ok

