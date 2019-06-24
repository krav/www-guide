module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Http

import Html exposing (Html, a, div, text, input, button)
import Html.Attributes exposing (class, id, placeholder, value, href, disabled)
import Html.Events exposing (onInput, onClick)

--import Event exposing (Event)
import Guide exposing (Guide, Events, Selection, makeSchedule, viewSchedule, viewEvents, search)
import Event exposing (Event, Category, Day)

-- TODO --

-- error handling - type..
-- loading screen

-- readme file

-- day starts at 7 am

-- favouriting
-- store list of strings in localstorage

-- filter: favourites
-- sorting
-- filter: all, category
-- filter: all, location
-- filter: all, family friendly
-- Automatic current date and time selector - use anchors

-- update index on search

-- Selecting bar: abs pos?
-- or: next prev day on bottom of page
-- get default day and time

-- .ics export -- make ical library?
-- .ics export favourites

-- Make special view for party camps

-- push notifications
-- chat

---- MODEL ----

type Error
    = NoErr
    | Error String

type alias Model =
    { guide : Maybe Guide
    , error : Error
    , search : String
    , selection : Selection
    }


init : ( Model, Cmd Msg )
init =
    ( { guide = Nothing
      , error = NoErr
      , search = ""
      , selection =
          { day = Event.Day "Monday 22." -- TODO
          , sorting = Guide.sortByTime
          , familyFriendly = False
          , category = Nothing
          }
    }, getCsv )



---- UPDATE ----


type Msg
    = GotCsv (Result Http.Error String)
    | Search String
    | SetDay Day

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCsv (Err e) ->
            ( { model | error = Error "HTTP error" }, getCsv )
        GotCsv (Ok s) ->
            case Guide.new s of
                (Ok g) ->
                    ( { model | guide = Just g } , Cmd.none )
                (Err e) ->
                    ( { model | error = Error "Parse error" } , Cmd.none )
        Search s ->
            ( { model | search = s }, Cmd.none )
        SetDay d ->
            let -- TODO
                oldS = model.selection
                s = { oldS | day = d}
            in
            ( { model | selection = s}, Cmd.none )

getCsv : Cmd Msg
getCsv =
    Http.get
        { url = "test.csv"
        , expect = Http.expectString GotCsv
        }

---- VIEW ----


view : Model -> Html Msg
view model =
    case model.guide of
        Nothing -> -- TODO
            div [] [ viewError model.error ]
        Just g ->
                div []
                    [ viewError model.error
                    , viewSelector g model.search model.selection.day
                    , if String.isEmpty model.search then
                        Guide.filter model.selection g.events
                        |> makeSchedule
                        |> viewSchedule
                      else
                        viewSearch model.search g ]
viewError : Error -> Html msg
viewError e =
    case e of
        NoErr -> text ""
        (Error s) -> text s

viewSearch : String -> Guide -> Html msg -- TODO move
viewSearch s g =
    case search s g of
        Err er ->
            (text ("Search error: " ++ er))
        Ok (index, matches) ->
            List.map (\(match, score) ->
                          List.filter (\e ->
                                           String.contains match e.id)
                          g.events)
            matches |> List.concat |> viewEvents

viewSelector : Guide -> String -> Day -> Html Msg -- TODO move
viewSelector g search day =
    div [ class "selector" ]
        [ div [ class "sel-days", class "sel-row" ]
               (List.map (\d ->
                              Html.button
                              [ class "day"
                              , onClick (SetDay d)
                              , Html.Attributes.disabled (day == d)]
                              [ text (Event.dayToShortString d) ])
                    g.days)
        , div [ class "sel-row" ]
            [ div [] [ input [ placeholder "Search", value search, onInput Search ] []
                     , Html.button [ Html.Attributes.disabled (String.isEmpty search)
                                   , onClick (Search "") ] [ text "Clear" ]]
            , a [ href "https://github.com/krav/www-guide" ] [ text "About" ]
            ]
        ]


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
