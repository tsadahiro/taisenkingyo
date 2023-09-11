port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events as Ev
import Json.Decode as D
import Html.Events.Extra.Pointer as Pointer
import Time
import Random
import Types exposing (..)


pondWidth = 800
pondHeight = 800

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- PORTS



port sendXY : Player  ->  Cmd msg
port sendKingyo : (List Kingyo) -> Cmd msg
port caught : {kingyos:(List Kingyo), points:Int, id:String} -> Cmd msg
port join : String -> Cmd msg
port skywayId : ({id:String, num:Int} -> msg) -> Sub msg
port moveInfo : (Player -> msg) -> Sub msg
port kingyoInfo : (List Kingyo -> msg) -> Sub msg
port kingyoCaught : ({kingyos:(List Kingyo), points:Int, id:String} -> msg) -> Sub msg

init : () -> ( Model, Cmd Msg )
init flags =
  ( { room = ""
    , name = ""
    , host = False
    , moving = False
    , x = 400, y = 400
    , points = 0
    , players = []
    , id = Nothing
    , kingyos = []
    }
  , Cmd.none
  )

randomKingyo: Random.Generator Kingyo
randomKingyo =
    Random.map5 
        (\x y vx vy level -> Kingyo (Vec2D x y) (Vec2D vx vy) level)
        (Random.int 0 799)
        (Random.int 0 799)
        (Random.int 6 15)
        (Random.int 6 15)
        (Random.int 1 5)
            
kingyoStep : Kingyo -> Kingyo
kingyoStep kingyo =
    let
        newVx = if (kingyo.pos.x+kingyo.v.x > pondWidth) ||
                    (kingyo.pos.x+kingyo.v.x < 0) then
                    -kingyo.v.x
                else
                    kingyo.v.x
        newVy = if kingyo.pos.y+kingyo.v.y > pondHeight ||
                    kingyo.pos.y+kingyo.v.y < 0 then
                    -kingyo.v.y
                else
                    kingyo.v.y

        newPos = Vec2D (kingyo.pos.x + kingyo.v.x)
                        (kingyo.pos.y + kingyo.v.y)
    in
        {kingyo| pos = newPos, v = Vec2D newVx newVy}
    


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      RoomChanged room ->
          ( { model | room = room }
          , Cmd.none
          )
        
      NameChanged name ->
          ( { model | name = name }
          , Cmd.none
          )
      KingyoGenerated newKingyos -> 
          ({model| kingyos = newKingyos++model.kingyos}
          , Cmd.none)
      Tick t -> ({model | kingyos = List.map kingyoStep model.kingyos}
                , if model.host then
                      sendKingyo model.kingyos
                  else
                      Cmd.none
                )
      Join ->
          ( model
          , (join model.room)
          )
      IdDefined info -> ({model | id = Just info.id
                         ,host = (info.num == 1)
                         }
                        ,if info.num == 1 then
                             Random.generate
                                 KingyoGenerated (Random.list 5 randomKingyo)
                         else
                             Cmd.none
                        )
      Recv info ->
          let
              exist = Debug.log "move" (0 < (List.length <| List.filter (\p -> p.name == info.name) model.players))
          in
              (if exist then
                   {model | players = List.map
                        (\p -> if p.name == info.name then
                                   {p | x = info.x, y = info.y}
                               else
                                   p
                        ) model.players
                   }
               else
                   {model | players = model.players ++ [info]}
              , Cmd.none
              )
      KingyoMoved kingyos -> ({model|kingyos=kingyos}, Cmd.none)
      KingyoCaught data ->
          let
              dummy = Debug.log "" data
              updatedPlayers = List.map
                               (\p -> if p.id == data.id then
                                          {p| points=data.points}
                                      else
                                          p
                               ) model.players
          in
              if model.host then
                  ({model | kingyos = data.kingyos
                   ,players = updatedPlayers}
                  , sendKingyo data.kingyos)
              else
                  ({model | players = updatedPlayers}
                  , Cmd.none)
      Down (x, y) ->
          let
             dummy =  Debug.log "down:" (x,y)
          in
              ({model | moving = True}, Cmd.none)

      Up (x,y) ->
          let
              newKingyos = sukuu (x,y) model.kingyos
              gained = (List.length model.kingyos) - (List.length newKingyos) 
          in
              if model.host then
                  ({model | moving = False
                   , points = model.points + gained
                   , kingyos = sukuu (x,y) model.kingyos
                   }
                  ,caught {kingyos = newKingyos
                           ,points = model.points+gained
                           ,id = case model.id of
                                     Nothing -> ""
                                     Just id -> id
                           }
                  )
              else
                  ({model | moving = False
                   , points = model.points + gained
                   }
                  , caught {kingyos = newKingyos
                           ,points = model.points+gained
                           ,id = case model.id of
                                     Nothing -> ""
                                     Just id -> id
                           }
                  )
          
      Move (x,y) ->
              if model.moving then
                  case model.id of
                      Just id ->
                          ({model | x = x, y = y}
                          ,sendXY {name=model.name, x=x, y=y, id=id, points=model.points}
                          )
                      Nothing ->
                          (model, Cmd.none)
              else
                  (model, Cmd.none)

sukuu: (Float, Float) -> (List Kingyo) -> (List Kingyo)
sukuu (x,y) kingyos =
    List.filter
        (\k -> (sqrt (((toFloat k.pos.x)-x)^2+((toFloat k.pos.y)-y)^2)) > 100)
            kingyos
                  
relativePos : Pointer.Event -> ( Float, Float )
relativePos event =
    event.pointer.offsetPos
        
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [moveInfo Recv
        ,kingyoInfo KingyoMoved
        ,kingyoCaught KingyoCaught
        ,skywayId IdDefined
        ,Time.every 100 Tick
        ]

-- VIEW


view : Model -> Html Msg
view model =
  div []
    (case model.id of
         Nothing -> [input
                         [ type_ "text"
                         , placeholder "Room"
                         , onInput RoomChanged
                         , on "keydown" (ifIsEnter Join)
                         , value model.room
                         ]
                         []
                    ,input
                         [ type_ "text"
                         , placeholder "Name"
                         , onInput NameChanged
                         , value model.name
                         ]
                         []
                    ,button [onClick Join] [text "Join"]
                    ]
         Just id -> [h1 [][text (String.fromInt model.points)]
                    ,div [] ([span [][text
                                          (String.fromInt (List.length model.players))
                                     ]
                             ]++(List.map pointView model.players))
                    ,Svg.svg [ Attr.width (String.fromInt pondWidth)
                             , Attr.height (String.fromInt pondHeight)
                             ]
                         ([Svg.rect [Attr.width "100%"
                                    ,Attr.height "100%"
                                    ,Attr.fill "skyblue"
                                    ]
                               []
                          ,Svg.circle [Attr.cx (String.fromFloat model.x)
                                      ,Attr.cy (String.fromFloat model.y)
                                      ,Attr.r "50"
                                      ,Attr.fill "white"
                                      ,Attr.stroke "black"
                                      ,Attr.fillOpacity (if model.moving then
                                                             "0.5"
                                                         else
                                                             "1"
                                                        )
                                      ,Attr.strokeDasharray "5,5"
                                      ,Attr.strokeWidth  "5"
                                      ,Pointer.onDown (relativePos >> Down) 
                                      ,Pointer.onUp (relativePos >> Up)
                                      ,Pointer.onMove (relativePos >> Move) 
                                      ][]
                          ]++(List.map amiView model.players)
                              ++ [Svg.rect [Attr.width "100%"
                                           ,Attr.height "100%"
                                           ,Attr.fill "none"
                                           ,Attr.stroke "black"
                                           ][]
                                 ]
                              ++(List.map kingyoView model.kingyos)
                         )
                    ])

pointView: Player -> Html Msg
pointView p =
    span [][text (p.name++":"++(String.fromInt p.points))
         ]
        
amiView: Player -> Html Msg
amiView p =
    Svg.g [][
         Svg.circle [Attr.cx (String.fromFloat p.x)
                    ,Attr.cy (String.fromFloat p.y)
                    ,Attr.r "50"
                    ,Attr.fill "white"
                    ,Attr.stroke "yellow"
                    ,Attr.strokeWidth  "5"
                    ][]
        ,Svg.text_ [Attr.x (String.fromFloat p.x)
                   ,Attr.y (String.fromFloat p.y)
                   ,Attr.fontSize "30"
                   ]
             [text p.name]
        ]


kingyoView: Kingyo -> Svg Msg
kingyoView kingyo =
    let
        vx = kingyo.v.x
        vy = kingyo.v.y    
        px = kingyo.pos.x
        py = kingyo.pos.y
        transtr = "(" ++ (String.fromInt kingyo.pos.x) ++ 
                    "," ++ (String.fromInt kingyo.pos.y) ++ ")"
        theta = String.fromFloat <| 
                if kingyo.v.x > 0 then
                    180/pi*(atan ((toFloat vy)/(toFloat vx)))
                else
                    180+180/pi*(atan ((toFloat vy)/(toFloat vx)))
    in
    Svg.g [Attr.transform (
                "rotate(" ++ theta ++  
                "," ++ (String.fromInt px) ++ 
                "," ++ (String.fromInt py) ++
                ")" ++
                "translate" ++ transtr
                )
                ]
        [Svg.path [Attr.d "m 0 20 l 30 -20 l -30 -20 l -60 26 l 0 -12 z"
                , Attr.fill "red"
                , Attr.stroke "red"
              ]
            []
        ,Svg.circle [Attr.cx "15"
                    ,Attr.cy "10"
                    ,Attr.r "10"
                    ,Attr.fill "white"
                ][]
        ,Svg.circle [Attr.cx "15"
                    ,Attr.cy "10"
                    ,Attr.r "8"
                    ,Attr.fill "black"
                    ][]
        ,Svg.circle [Attr.cx "15"
                    ,Attr.cy "-10"
                    ,Attr.r "10"
                    ,Attr.fill "white"
                    ][]        
        ,Svg.circle [Attr.cx "15"
                ,Attr.cy "-10"
                ,Attr.r "8"
                ,Attr.fill "black"
                ][]
        ,Svg.circle [Attr.cx "18"
                ,Attr.cy "10"
                ,Attr.r "2"
                ,Attr.fill "white"
                ][]
        ,Svg.circle [Attr.cx "18"
                ,Attr.cy "-10"
                ,Attr.r "2"
                ,Attr.fill "white"
                ][]
        ,Svg.path [Attr.d "m 0 20 l -10 10 l -10 -5 z"
            , Attr.fill "red"
            ][]
        ,Svg.path [Attr.d "m 0 -20 l -10 -10 l -10 5 z"
            , Attr.fill "red"
            ][]
        ]

-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
  D.field "key" D.string
    |> D.andThen (\key -> if key == "Enter" then D.succeed msg else D.fail "some other key")
