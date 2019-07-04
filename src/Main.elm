module Main exposing (..)

import Browser
import Http
import Html exposing (Html, a, div, text, input, button, h1)
import Html.Attributes exposing (class, id, src, placeholder, value, href, disabled)
import Html.Events exposing (onInput, onClick)

import Guide exposing (Guide, Events, Selection, makeSchedule, viewSchedule, viewEvents, search)
import Event exposing (Event, Category, Day)

import Url
import Url.Parser as UP exposing ((<?>))
import Url.Parser.Query as Query
import Browser.Navigation as Nav

import Color
import Markdown

---- MODEL ----

type Error
    = NoErr
    | Error String

type Route
    = AboutPage
    | DayPage Event.Day

type alias Model =
    { key : Nav.Key
    , guide : Maybe Guide
    , error : Error
    , search : String
    , selection : Selection
    , route : Maybe Route
    }

init : String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init csv url key =
    ( { key = key
      , guide = Nothing
      , error = NoErr
      , search = ""
      , selection =
          { day = Event.Day "Monday 22."
          , sorting = Guide.sortByTime
          , familyFriendly = False
          , category = Nothing
          }
      , route = UP.parse routeParser url
    }, getCsv csv )

routeParser : UP.Parser (Route -> Route) Route
routeParser
    = UP.oneOf
      [ UP.map AboutPage (UP.s "about")
      , UP.map (\s -> DayPage <| Event.Day <| Maybe.withDefault "Monday 22." s ) (UP.top <?> Query.string "day")
      , UP.map (\s -> DayPage <| Event.Day <| Maybe.withDefault "Monday 22." s ) (UP.s "index.html" <?> Query.string "day")
      ]

---- UPDATE ----

type Msg
    = GotCsv (Result Http.Error String)
    | Search String
    | SetDay String
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCsv (Err e) ->
            ( { model | error = Error "HTTP error" }, Cmd.none )
        GotCsv (Ok s) ->
            case Guide.new s of
                (Ok g) ->
                    ( { model | guide = Just g } , Cmd.none )
                (Err e) ->
                    ( { model | error = Error "Parse error" } , Cmd.none )
        Search s ->
            ( { model | search = s }, Cmd.none )
        SetDay d ->
            ( model, Nav.pushUrl model.key ("/?day=" ++ d) )
        UrlChanged url ->
            ( { model | route = UP.parse routeParser url }
            , Cmd.none )
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url) )
                Browser.External href ->
                    ( model, Nav.load href )

getCsv : String -> Cmd Msg
getCsv url =
    Http.get
        { url = url
        , expect = Http.expectString GotCsv }

---- VIEW ----

view : Model -> Browser.Document Msg
view model =
    case model.route of
        Nothing ->
            { title = "What"
            , body = [ Html.img [ Html.Attributes.src "https://media.giphy.com/media/S5JSwmQYHOGMo/giphy.gif"] [] ] }
        Just page ->
            case model.guide of
                Nothing ->
                    { title = "Borderland Guide"
                    , body = [ viewError model.error
                             , h1 [] [ text "Please Wait" ]
                             , text "Mining bitcoin ..." ] }
                Just guide ->
                    viewPage model guide page

viewPage : Model -> Guide -> Route -> Browser.Document Msg
viewPage model guide route =
    case route of
        AboutPage ->
            { title = "Borderland Guide - About"
            , body = viewAbout guide
            }
        DayPage d ->
            if String.length model.search < 3 then
                { title = "Borderland Guide - " ++ (Event.dayToString d)
                , body = [ viewSelector guide.days model.search d
                         , Guide.filter d model.selection guide.events
                         |> makeSchedule
                         |> viewSchedule
                      ]}
            else
                { title = "Borderland Guide - " ++ model.search
                , body = [ viewSelector guide.days model.search d
                         , viewSearch model.search guide
                         ]}

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
                          List.filter (\e -> String.contains match e.id) -- TODO inefficient
                          g.events)
            (List.take 50 matches) |> List.concat |> viewEvents

viewSelector : List Event.Day -> String -> Day -> Html Msg -- TODO move
viewSelector days search day =
    div [ class "selector" ]
        [ div [ class "sel-row" ]
              [ Html.select [ Html.Events.onInput SetDay ]
               (List.map (\d ->
                              Html.option
                              [ class "day"
                              , Html.Attributes.selected (day == d)]
                              [ text (Event.dayToString d) ])
                days)
              , div [] [ input [ placeholder "Search", value search, onInput Search ] []
                       , Html.button [ Html.Attributes.disabled (String.isEmpty search)
                                     , onClick (Search "") ] [ text "Clear" ]]
           -- , div [] [ input [ Html.Attributes.type_ "checkbox" ] []
           --          , text "Only Favourites" ] -- TODO text for checkbox
              , a [ href "/about" ] [ text "About" ]
            ]
        ]

viewAbout : Guide -> List (Html msg)
viewAbout g =
    [ div [ class "selector" ]
          [ div [ class "sel-row" ]
                [ a [ href "/"] [ text "<- Back" ]
                , a [ href "https://menu.theborderland.se"] [ text "To Other Borderland Sites ->" ]]
          ]
    , Markdown.toHtml [] """
# Borderland That There Then Guide
Add this page to your **homescreen** *so it's almost like an app:* Android - with a newish Chrome click the button at the bottom of the screen. Apple - From the Share menu select "Add to Homescreen". This thing works without internet, so put your phone in airplane mode after.

You can submit new events and edit old ones by [using our form](https://docs.google.com/forms/d/e/1FAIpQLSe_LzyEiLiryK-R_y3lgCtdeYIhQC2sTBsWH38WHL_dF2_ptA/viewform), it can take a few hours for this page to update.

Only you can prevent this site from sucking by [making pull requests](https://github.com/krav/www-guide).

Be Gay, Do Crime!

## Colour coding and emoji explained
Bottom colour is camp colour. 🧸 is child friendly.

"""
    , div [] <| List.map (\c -> -- TODO move
                              div [ class "event"
                                  , Html.Attributes.style "background" <| Color.toRGBString <| Event.categoryToColor c ]
                              [ div [ class "title" ] [ text <| Event.categoryToEmoji c
                                                      , text <| Event.categoryToString c ] ]
                         ) Event.categoryEnum
    ]
---- PROGRAM ----

main : Program String Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
