import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Signal exposing(..)
import Signal.Time exposing (settledAfter)
import Window
import Random

(gameWidth,gameHeight) = (800,400)
(halfWidth,halfHeight) = (400,200)

maxVelocity = 200

type State = Play | Pause | PauseReset | RiskScored | SalesScored

type alias Ball =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }

type alias Player =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , score : Int
    }

type alias Goal = 
    { x : Float
    , y : Float
    }

type alias Game =
    { space : Bool
    , state : State
    , ball : Ball
    , player1 : Player
    , player2 : Player
    , goal1 : Goal
    , goal2 : Goal
    , deltaState : Float
    }

type alias Direction = 
    { y : Int
    , x : Int
    }

type alias Velocity = 
    { vy : Float
    , vx : Float
    }

type alias Input =
    { spaceNew : Bool
    , dir1 : Direction
    , dir2 : Direction
    , delta : Time
    }


player : Float -> Player
player x = Player x 0 0 0 0

goal : Float -> Goal
goal x = Goal x 0

newBallPlease : Ball
newBallPlease = Ball 0 0 0 0 

defaultGame : Game
defaultGame =
    { space = False
    , state = PauseReset
    , ball = newBallPlease
    , player1 = player (35-halfWidth)
    , player2 = player (halfWidth-35)
    , goal1 = goal (10-halfWidth)
    , goal2 = goal (halfWidth-10)
    , deltaState = 0
    } 

-- UPDATE

checkSpaceBarState : Bool -> Bool -> Bool
checkSpaceBarState old new = old == True && new == False

near : Float -> Float -> Float -> Bool
near posA hitBox posB = posB >= posA - hitBox && posB <= posA + hitBox

within obj ball sizex sizey = near obj.x (sizex / 2) ball.x && near obj.y (sizey / 2) ball.y

update : Input -> Game -> Game
update {spaceNew,dir1,dir2,delta} ({space,state,ball,player1,player2,goal1,goal2,deltaState} as game) =
  let
    openOrCloseMenu = 
        checkSpaceBarState space spaceNew

    score1 =
        if within goal2 ball 15 72 then 1 else 0

    score2 =
        if within goal1 ball 15 72 then 1 else 0

    newState =
        if openOrCloseMenu && (state == RiskScored || state == SalesScored) then
            PauseReset
        else if openOrCloseMenu && (state == Pause || state == PauseReset) then
            Play
        else if openOrCloseMenu && state == Play then        
            Pause
        else if score1 > score2 then
            RiskScored
        else if score2 > score1 then
            SalesScored
        else
            state

    newDelta =
        if score1 /= score2 || state == Play || state == Pause 
        then 0
        else deltaState + 10

    newBall =
        if newState == RiskScored || newState == SalesScored then newBallPlease
        else if state == Pause then ball 
        else updateBall delta ball player1 player2

    player1New = 
        if state == PauseReset then resetPlayer score1 (35-halfWidth) player1
        else if state == Pause then player1
        else updatePlayer delta dir1.y dir1.x score1 player1

    player2New = 
        if state == PauseReset  then resetPlayer score2 (halfWidth-35) player2
        else if state == Pause then player2
        else updatePlayer delta dir2.y dir2.x score2 player2

    in
        { game |
            space = spaceNew,
            state = newState,
            ball = newBall,
            player1 = player1New,
            player2 = player2New,
            goal1 = goal1,
            goal2 = goal2,
            deltaState = newDelta   
        }

playerImpetus : Player -> Ball -> Velocity
playerImpetus player ball =      
    if within player ball 40 40 then 
        { vx = 0.1 * player.vx
        , vy = 0.1 * player.vy
        }        
    else 
        { vx = 0
        , vy = 0
        } 

limitMaxVelocity : Float -> Float
limitMaxVelocity velocity = 
    if velocity > maxVelocity 
    then maxVelocity
    else velocity

updateBall : Time -> Ball -> Player -> Player -> Ball
updateBall dt ball player1 player2 =
    let 
        wallCollide = ball.x < -halfWidth || ball.x > halfWidth || ball.y < 7 - halfHeight || ball.y > halfHeight
        wallVelocity = if wallCollide then 0.7 else 1
        p1i = playerImpetus player1 ball
        p2i = playerImpetus player2 ball
        p1RushKeeper = ball.x < -halfWidth || within player1 ball 40 40
        p2RushKeeper = ball.x > halfWidth || within player2 ball 40 40
    in
        updatePosition dt
            { ball |
                vx = limitMaxVelocity ((stepV ball.vx p1RushKeeper p2RushKeeper) + p1i.vx + p2i.vx) * wallVelocity,
                vy = limitMaxVelocity ((stepV ball.vy (ball.y < 7 - halfHeight) (ball.y > halfHeight - 7)) + p1i.vy + p2i.vy) * wallVelocity
            }

resetPlayer : Int -> Float -> Player -> Player
resetPlayer points start player = 
    { player |
        x = start,
        y = 0,
        score = player.score + points
    }

updatePlayer : Time -> Int -> Int -> Int -> Player -> Player
updatePlayer dt dirv dirh points player =
    let
        movedPlayer = updatePosition dt { player | 
            vy = toFloat dirv * 200,
            vx = toFloat dirh * 200
        }
    in
        { movedPlayer |
            y = clamp (12-halfHeight) (halfHeight-12) movedPlayer.y,
            x = clamp (12-halfWidth) (halfWidth-12) movedPlayer.x,
            score = player.score + points
        }

updatePosition dt obj =
    { obj |
        x = obj.x + obj.vx * dt,
        y = obj.y + obj.vy * dt
    }
  
stepV v lowerCollision upperCollision =
    if lowerCollision then abs v
    else if upperCollision then -(abs v)
    else v

-- VIEW CONSTANTS

txt f string rgb = Text.fromString string |> Text.color rgb |> Text.monospace |> f |> leftAligned
txtBold f string rgb = Text.fromString string |> Text.bold |> Text.color rgb |> Text.monospace |> f |> leftAligned

msg = "SPACE to start, WASD and &uarr;&larr;&darr;&rarr; to move"
title = "MarketInvoice Foo.bool"
pitchGreen = rgb 100 140 100
textGreen = rgb 160 200 160
field = rect gameWidth gameHeight |> filled pitchGreen
halfway = path [(0, -halfHeight), (0, halfHeight)] |> traced (dashed white)

goalArea = halfWidth / 4    
goalRed game = makeGoal game.goal1 red 10 72
goalRedLine = path 
            [ (halfWidth, -(goalArea))
            , (halfWidth - (goalArea), -(goalArea))
            , (halfWidth - (goalArea), (goalArea))
            , (halfWidth, (goalArea))
            ] |> traced (dashed white)        

goalBlue game = makeGoal game.goal2 blue 10 72
goalBlueLine = path 
            [ (-halfWidth, (goalArea))
            , (-halfWidth + (goalArea), (goalArea))
            , (-halfWidth + (goalArea), -(goalArea))
            , (-halfWidth, -(goalArea))
            ] |> traced (dashed white)

goalCelebration game text =  txtBold (Text.height 140) text (generateColor game.deltaState) |> toForm |> move (0, 10) |> rotate (degrees game.deltaState)
rejected game = goalCelebration game "REJECTED!"
launched game = goalCelebration game "LAUNCHED!"

menuItems  = 
    [ rect (1.7 * halfWidth) 40 |> filled white
    , txt (Text.height 24) msg black |> toForm
    , txt (Text.height 32) "Risk" red |> toForm |> move (-54, halfHeight - 40)
    , txt (Text.height 32) "Sales" blue |> toForm |> move (64, halfHeight - 40)
    , txtBold (Text.height 35) title black |> toForm |> move (0, 40)
    ]
    |> group
    |> move (0, 40 - halfHeight) -- Text

-- MAIN VIEW

view : (Int,Int) -> Game -> Element
view (w,h) game =
    let        
        scores = txt (Text.height 50) (toString game.player1.score ++ "  " ++ toString game.player2.score) white
    in
        container w h middle <|
        collage gameWidth gameHeight
        [ field
        , halfway
        , goalRedLine
        , goalBlueLine
        , oval 15 15 |> make game.ball white -- Ball
        , makePlayer game.player1 red 
        , makePlayer game.player2 blue
        , goalRed game
        , goalBlue game
        , toForm scores
          |> move (0, halfHeight - 40) -- Scores
        , if game.state == Play 
          then spacer 1 1 |> toForm
          else menuItems
        , if game.state == RiskScored
          then rejected game
          else spacer 1 1 |> toForm
        , if game.state == SalesScored
          then launched game
          else spacer 1 1 |> toForm
        ]

-- Generates random colors
generateColor gen =
    let 
        (i, _) = numGen 0 3 gen
    in
        case round gen % 3 of
            0 -> orange
            1 -> yellow
            2 -> red
            3 -> green
            _ -> blue

numGen : Int -> Int -> Float -> (Int, Random.Seed)
numGen from to rand = Random.generate (Random.int from to) (Random.initialSeed (round rand))

make obj rgb shape = shape |> filled rgb |> move (obj.x, obj.y)

makeGoal obj rgb width height = 
    [ rect (width + 2) (height + 2) |> filled white
    , path 
        [ (width / 2, height / -2)
        , (width / -2, height / -2)
        , (width / -2, height / 2)
        , (width / 2, height / 2)
        , (width / 2, height / -2)
        ]
      |> traced (solid rgb)
    ]
    |> group
    |> move (obj.x, obj.y)

makePlayer obj rgb =
    let 
        (number, _) = numGen 2 15 42
    in
        [ rect 20 26 |> filled rgb
        , rect 8 8  |> filled rgb |> move (-12, 8)
        , rect 8 8 |> filled rgb |> move (12, 8)
        , toForm (txt identity (toString number) white) |> move (0, 4)
        ] 
        |> group
        |> move (obj.x, obj.y)


-- SIGNALS

gameState : Signal Game
gameState =
    Signal.foldp update defaultGame input

delta =
    Signal.map inSeconds (fps 35)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
        Keyboard.space
        Keyboard.wasd
        Keyboard.arrows
        delta

main =
  Signal.map2 view Window.dimensions gameState