module Types exposing (..)

import Time

type Msg = RoomChanged String
         | NameChanged String
         | Recv Player
         | Join
         | IdDefined {id:String, num:Int}
         | Down (Float, Float)
         | Move (Float, Float)
         | Up (Float, Float)
         | KingyoGenerated (List Kingyo)
         | KingyoMoved (List Kingyo)
         | KingyoCaught {kingyos:(List Kingyo), points:Int, id:String}
         | Tick Time.Posix

type alias Player = {name: String
                    ,x: Float
                    ,y: Float
                    ,id: String
                    ,points: Int
                    }
type alias Model =
  { room : String
  , name : String
  , moving : Bool
  , id : Maybe String
  , host : Bool
  , x : Float
  , y : Float
  , points : Int
  , players : List Player
  , kingyos : List Kingyo
  }

type alias Vec2D = {x: Int
                    ,y: Int
                    }
type alias Kingyo = {pos: Vec2D
                    ,v: Vec2D
                    ,level: Int
                    }

type alias Ami = {pos : Vec2D}
