module Event exposing (..)

import Clock exposing (Time)


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
    }


categoryEnum = [ Category "Fun"
               , Category "Not Fun"
               ]
categoryToString (Category c) = c

dayToString (Day c) = c


