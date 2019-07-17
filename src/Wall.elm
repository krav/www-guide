module Wall exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

viewWall : List (Html msg)
viewWall = [ div [ class "iframe-container"]
                 [ iframe [ class "iframe-embed"
                          , name "embed_readwrite"
                          , src "https://board.net/p/piefohJiepha9mei6quaizi8eshaiG3weisho4da3vahfug8wi?showControls=true&showChat=true&showLineNumbers=false&useMonospaceFont=false" ] [] ]]

