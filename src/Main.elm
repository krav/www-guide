port module Main exposing (..)

import Browser
import Http
import Html exposing (Html, a, div, text, input, button, h1, label)
import Html.Attributes exposing (class, id, src, placeholder, value, href, disabled, type_, checked)
import Html.Events exposing (onInput, onClick)

import Set exposing (Set)

import Url
import Url.Parser as UP exposing ((<?>))
import Url.Parser.Query as Query
import Browser.Navigation as Nav

import Color
import Markdown

import Guide exposing (Guide, Events, Selection, makeSchedule, viewSchedule, viewEvents, search)
import Event exposing (Event, Category, Day, Msg, eventCsv)
import Map exposing (viewMap)

import Json.Encode as E
import File.Download as Download

port storeFavs : E.Value -> Cmd msg

---- MODEL ----

type Error
    = NoErr
    | Error String

type Route
    = AboutPage
    | DayPage Event.Day
    | MapPage
    | WallPage

type alias Model =
    { key : Nav.Key
    , guide : Maybe Guide
    , error : Error
    , search : String
    , selection : Selection
    , route : Maybe Route
    , favs : Set String
    , wall : String
    }
type alias Flags = (String, List String)

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init (csv, favs) url key =
    ( { key = key
      , guide = Nothing
      , error = NoErr
      , search = ""
      , selection =
          { sorting = Guide.sortByTime
          , familyFriendly = False
          , category = Nothing
          , onlyFavs = False
          }
      , route = UP.parse routeParser url
      , favs = Set.fromList favs
      , wall = "Fetching wall from the Internet ..."
    }, getCsv csv )

routeParser : UP.Parser (Route -> Route) Route
routeParser
    = UP.oneOf
      [ UP.map AboutPage (UP.s "about")
      , UP.map (\s -> DayPage <| Event.Day <| Maybe.withDefault "Monday 22." s ) (UP.top <?> Query.string "day")
      , UP.map (\s -> DayPage <| Event.Day <| Maybe.withDefault "Monday 22." s ) (UP.s "index.html" <?> Query.string "day")
      , UP.map MapPage (UP.s "map")
      , UP.map WallPage (UP.s "wall")
      ]

---- UPDATE ----

type Msg
    = GotCsv (Result Http.Error String)
    | Search String
    | SetDay String
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | UpdateEvent Event.Msg
    | ToggleOnlyFavs
    | ExportCsv
    | GetWall
    | GotWall (Result Http.Error String)

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
        UpdateEvent (Event.Fav f) ->
            let
                newFavs = Set.insert f model.favs
            in
            ( { model | favs = newFavs }
            , E.set E.string newFavs |> storeFavs )
        UpdateEvent (Event.UnFav f) ->
            let
                newFavs = Set.remove f model.favs
            in
            ( { model | favs = newFavs }
            , E.set E.string newFavs |> storeFavs )
        ToggleOnlyFavs ->
            let
                selection = model.selection
                newSelection = { selection | onlyFavs = not model.selection.onlyFavs }
            in
            ( { model | selection = newSelection }, Cmd.none)
        ExportCsv ->
            case model.guide of
                Just g ->
                    ( model, Download.string "guide.csv" "text/csv" <| String.join "\n" <| List.map eventCsv g.events)
                Nothing ->
                    ( model, Cmd.none )
        GetWall ->
            ( model, getWall "https://board.net/p/piefohJiepha9mei6quaizi8eshaiG3weisho4da3vahfug8wi/export/markdown" )
        GotWall r ->
            ( { model | wall = Result.withDefault "Unable to fetch wall" r }
            , Cmd.none )

getWall : String -> Cmd Msg
getWall url =
    Http.get { url = url
             , expect = Http.expectString GotWall }

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
                             , div [ class "loading" ] [ text "Getting ready ..." ]
                             ]
                    }
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
                , body = [ viewSelector model.selection.onlyFavs guide.days model.search d
                         , div [ class "main" ] [ Guide.filter d model.favs model.selection guide.events
                         |> makeSchedule
                         |> viewSchedule model.favs
                         |> fromEventMsg
                                                   ]
                      ]}
            else
                { title = "Borderland Guide - " ++ model.search
                , body = [ viewSelector model.selection.onlyFavs guide.days model.search d
                         , div [class "main" ] [ viewSearch model.favs model.search guide |> fromEventMsg ]
                         ]}
        MapPage ->
            { title = "Borderland Guide - Map"
            , body = [ div [ class "selector" ]
                           [ div [ class "left" ]
                                 [ a [ href "/"] [ text "Back" ]]]
                     , div [ class "main" ] viewMap ]}
        WallPage ->
            { title = "Borderland Guide - Wall"
            , body = [ div [ class "selector" ]
                           [ div [ class "left" ]
                                 [ a [ href "/"] [ text "Back" ]]
                           , div [ class "right" ]
                                 [ a [ href "https://board.net/p/piefohJiepha9mei6quaizi8eshaiG3weisho4da3vahfug8wi"
                                     , Html.Attributes.target "_blank" ] [ text "Edit" ]]]
                     , div [ class "main" ] (viewWall model) ]}


viewWall : Model -> List (Html msg)
viewWall m = [ Markdown.toHtml [ class "wall" ] m.wall ]


fromEventMsg : Html Event.Msg -> Html Msg
fromEventMsg
    = Html.map (\em -> UpdateEvent em)

viewError : Error -> Html msg
viewError e =
    case e of
        NoErr -> text ""
        (Error s) -> text s

viewSearch : Set String -> String -> Guide -> Html Event.Msg -- TODO move
viewSearch favs s g =
    case search s g of
        Err er ->
            (text ("Search error: " ++ er))
        Ok (index, matches) ->
            List.map (\(match, score) ->
                          List.filter (\e -> String.contains match e.id) -- TODO inefficient
                          g.events)
            (List.take 50 matches) |> List.concat |> viewEvents favs

viewSelector : Bool -> List Event.Day -> String -> Day -> Html Msg -- TODO move
viewSelector onlyFavs days search day =
    div [ class "selector" ]
        [ div [ class "left" ]
              [ Html.select [ Html.Events.onInput SetDay ]
               (List.map (\d ->
                              Html.option
                              [ class "day"
                              , Html.Attributes.selected (day == d)]
                              [ text (Event.dayToString d) ])
                days)
              , Html.span [] [input [ placeholder "Search"
                      , class "search"
                      , value search
                      , onInput Search ] []
              , Html.button [ Html.Attributes.disabled (String.isEmpty search)
                            , onClick (Search "") ] [ text "Clear" ]]
              , Html.span [] [label [] [ input
                                [ type_ "checkbox"
                                , checked onlyFavs
                                , onClick ToggleOnlyFavs ]
                                []
                          , text "💖"]]
            ]
        , div [ class "right" ]
            [ a [ href "/wall"
                , onClick GetWall ] [ text "Wall" ]
            , a [ href "/map" ] [ text "Map" ]
            , a [ href "/about" ] [ text "About" ] ]
        ]

viewAbout : Guide -> List (Html Msg)
viewAbout g = -- TODO move
    [ div [ class "selector" ]
          [ div [ class "left" ]
                [ a [ href "/"] [ text "Back" ]]
          , div [ class "right" ] [ a [ href "https://menu.theborderland.se"
                                      , Html.Attributes.target "_blank" ]
                                        [ text "To Other Borderland Sites" ]
                                  ]]
    , Markdown.toHtml [ class "main" ] """
# Borderland That There Then Guide
Add this page to your **homescreen** *so it's almost like an app:*

  * Android - with a newish Chrome click the button at the bottom of the screen.
  * Apple - From the Share menu select "Add to Homescreen".

This thing works without internet, so put your phone in airplane mode after.

You can submit new events and edit old ones by [using our form](https://docs.google.com/forms/d/e/1FAIpQLSe_LzyEiLiryK-R_y3lgCtdeYIhQC2sTBsWH38WHL_dF2_ptA/viewform), it can take a few hours for this page to update.

Only you can prevent this site from sucking by [making pull requests](https://github.com/krav/www-guide).

🏴 Be Gay, Do Crime! 🏴

## Colour coding and emoji explained
Bottom colour is camp colour. 🧸 is child friendly.

"""
    , div [] <| List.map (\c -> -- TODO move
                              div [ class "colordesc"
                                  , Html.Attributes.style "background" <| Color.toRGBString <| Event.categoryToColor c ]
                              [ div [ class "title" ] [ text <| Event.categoryToEmoji c
                                                      , text "   "
                                                      , text <| Event.categoryToString c ] ]
                         ) Event.categoryEnum
    , div [] [ Html.h2 [] [ text "Other formats" ]
             , button [ onClick ExportCsv ] [ text "Download CSV"]
   ]
    ]
---- PROGRAM ----

main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
