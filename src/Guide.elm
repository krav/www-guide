module Guide exposing (..)

import Csv exposing (Csv, parse)
import Csv.Decode as CD exposing (Decoder, map, andMap, decode, field)
import Http
import List.Extra
import Clock exposing (RawTime, Time, fromRawParts, getHours, getMinutes)
import ElmTextSearch as ETS
import Html exposing (Html, a, div, text, h2, h3, h4, input)
import Html.Attributes exposing (class, id)
import Set exposing (Set)

import Event exposing (Event, Category, Day, viewEvent)

-- Model

type alias Events = List Event

type alias DaySchedule = List (Int, Events) -- (hour, events)

type alias Selection =
    { sorting : (Event -> Event -> Order)
    , familyFriendly : Bool
    , category : Maybe Category
    , onlyFavs : Bool
    --, location : Maybe String
    }

type alias Guide =
    { events : Events
    , index : ETS.Index Event
    , days : List Day
    }

-- String to guide

new : String -> Result CD.Errors Guide
new s
    = parseCsv s |>
      Result.andThen (\e -> Ok
                          { events = e
                          , index = buildIndex e
                          , days = getDays e })
-- Days in event, sorted

getDays : Events -> List Day
getDays _ = -- TODO
    --List.map (\e -> .day es |> List.Extra.uniqueBy (\(Event.Day s) -> s)
    [ (Event.Day "Monday 22."), (Event.Day "Tuesday 23."), (Event.Day "Wednesday 24."), (Event.Day "Thursday 25."), (Event.Day "Friday 26."), (Event.Day "Saturday 27."), (Event.Day "Sunday 28.")]

-- Gather events by hour, -1 is all day
makeSchedule : Events -> DaySchedule
makeSchedule es =
    let
      (allDay,rest) = List.partition (\e -> e.allDay) es
    in
      (-1, allDay)::(
          List.Extra.gatherWith (\e1 e2 ->
                                  getHours e1.time == getHours e2.time)
          rest
      |> List.map (\(a, b) -> (getHours a.time, a::b)))

-- Filter events by selection
filter : Day -> Set String -> Selection -> Events -> Events
filter d favs s e =
    filterByDay d e
    |> filterFavs favs s
    |> List.sortWith s.sorting

filterFavs : Set String -> Selection -> Events -> Events
filterFavs favs s es =
    if s.onlyFavs then
        List.filter (\e -> Set.member e.id favs) es
    else
        es

-- Sorting

sortByTime : Event -> Event -> Order
sortByTime e1 e2 = Clock.compare e1.time e2.time

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
    --ETS.addDocs es newIndex |> Tuple.first TODO use cache
    ETS.addDocs es newIndex |> Tuple.first

search : String -> Guide -> Result String ( ETS.Index Event, List (String, Float))
search s g =
    ETS.search s g.index

-- Views

viewSchedule : Set String -> DaySchedule -> Html Event.Msg
viewSchedule favs ds_ =
    let
        ds = List.sortBy Tuple.first ds_
    in
    div [] (List.map (\(h, es) ->
                  let
                      hour = if h == -1 then -- Fix this
                                 "All day"
                             else
                                 (String.fromInt h |> String.pad 2 '0')++":00"
                  in
                  div [ ]
                      [ div [ class "hour" ] [ a [ id hour ] [ h2 [] [ text hour ] ]]
                      , viewEvents favs es ] )
                ds)

viewEvents : Set String -> Events -> Html Event.Msg
viewEvents favs es =
    div [ class "events" ] (List.map (viewEvent favs) es)

-- Decode events CSV

parseCsv : String -> Result CD.Errors Events
parseCsv s = -- CD.decode expects a different error
    Csv.parse s |> Result.withDefault ({headers = [], records = []}) |> CD.decodeCsv decodeEvents

decodeEvents : Decoder (Event -> Event) Event
decodeEvents =
        map Event (field "Timestamp" Ok
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
                  |> andMap (field "All-day events" <| \s ->
                            Ok (String.contains "all-day" s))
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
