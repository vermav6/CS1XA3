module Assign2 exposing (..)

import Html exposing (..)
import Html.Events exposing (on, keyCode)
import Keyboard
import Char
import Random
import Window
import Task
import Time exposing (Time, millisecond)
import Color
import Element
import Collage

-- The dimenstions of the grid and the snake

columns    = 44
rows       = 25
width_of_snake = 14
width      = width_of_snake * columns
height     = width_of_snake * rows


-- Creating lists and feature

lists : List a -> Maybe a
lists list = case list of
              []    -> Nothing 
              x::[] -> Just x
              x::xs -> lists xs



-- Overall design

-- Positions and points
type alias Pos = (Int, Int)

newPosition : Int -> Int -> Int 
newPosition post maxpost = if post== maxpost then 0 else post + 1 

up : Int -> Int
up x = newPosition x columns

rightside : Int -> Int
rightside y = newPosition y rows

randPost : Random.Generator Pos
randPost = Random.pair (Random.int 0 (columns-1)) (Random.int 0 (rows-1))
    
-- Directions of snake along with sense and conditioning
type Direction = Up | Down | Left | Right

opposite : Direction -> Direction -> Bool
opposite d1 d2 =
    case (d1, d2) of
      (Left,Right) -> True
      (Right,Left) -> True
      (Up,Down)    -> True
      (Down,Up)    -> True
      _            -> False    
 
-- Fruit
pickup_fruit : Pos -> List Pos -> Pos
pickup_fruit fruit positions =
    if List.any (\snake -> snake == fruit) positions then
        let (fruitx , fruity ) = fruit
            (fruitx_, fruity_) = (rightside fruitx, up fruity)
            overflow = fruitx == columns
            post_     = (fruitx_, if overflow then fruity_ else fruity)
        in
          pickup_fruit post_ positions
    else
        fruit

-- Snake
type alias Snake = { positions: List Pos, dir: Direction }

stepSnake : Snake -> Snake
stepSnake { positions, dir } =
      let snakeHead = lists positions
      in 
        case snakeHead of
          Just (x, y) ->
              let x_ = x + case dir of
                             Left  -> -1
                             Right ->  1
                             _     ->  0
                  y_ = y + case dir of
                             Up   ->  1
                             Down -> -1
                             _    ->  0
              in
                { positions = positions ++ [(x_, y_)], dir = dir }
          Nothing -> { positions = positions, dir = dir } 

wall_collisions : Snake -> Bool
wall_collisions { positions, dir } =
    let snakeHead = lists positions
    in
      case snakeHead of 
        Just (x, y) -> 
            x < 0       ||
            x > columns ||
            y < 0       ||
            y > rows
        Nothing -> False

selfsnake_collisions : Snake -> Bool
selfsnake_collisions { positions, dir } =
    let snakeHead = lists positions
        otherPos  = List.drop 1 <| List.take (List.length positions - 1) positions
    in 
      case snakeHead of
        Just head -> List.any (\body -> body == head) otherPos
        Nothing   -> False

--Eating the fruit

eatFruit : Snake -> Pos -> Bool
eatFruit { positions, dir } fruit =
    let snakeHead = lists positions
    in case snakeHead of
         Just head -> head == fruit
         Nothing   -> False 

-- Score and semantics
type alias Game = { snake: Snake, fruit: Pos, window: Window.Size }

init : (Game, Cmd Msg)
init =
    let
        middle    = (\max -> round <| max / 2)
        midX      = middle columns
        midY      = middle rows
        positions = List.map (\i -> (i, midY)) <| List.reverse (List.range (midX-5) (midX+3))
        game = { snake = { positions = positions, dir = Left }
               , fruit = (-1, -1)
               , window = { width = 0, height = 0} }  
    in (game, Cmd.batch [ Random.generate NewFruit randPost
                        , Task.perform SetSize Window.size])

-- Update after picking up a fruit and when the game ends

type Msg = Tick Time | ChangeDirection Direction | NewFruit Pos | SetSize Window.Size | NoOp 

direction : Int -> Msg 
direction keyCode =
    case keyCode of
      37 -> ChangeDirection Left
      38 -> ChangeDirection Up
      39 -> ChangeDirection Right
      40 -> ChangeDirection Down
      _  -> NoOp

update : Msg -> Game -> (Game, Cmd Msg)
update msg model =
    case msg of
    
      Tick _ ->
          if wall_collisions model.snake || selfsnake_collisions model.snake then
              init
          else
              let snake_ = stepSnake model.snake
              in
                if eatFruit snake_ model.fruit then
                    ({model | snake = snake_}, Random.generate NewFruit randPost)
                else
                    ({ model | snake = { snake_ | positions = List.drop 1 snake_.positions }}, Cmd.none)
  
      ChangeDirection direction -> 
          let direction_ =
                  if opposite direction model.snake.dir 
                  then model.snake.dir 
                  else direction
              snake = model.snake
          in
            ({ model | snake = { snake | dir = direction_}}, Cmd.none)
    
      NewFruit pos -> 
          ({ model | fruit = pickup_fruit pos model.snake.positions }, Cmd.none)

      SetSize windowSize -> 
          ({ model | window = windowSize}, Cmd.none)

      NoOp -> (model, Cmd.none)


subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every (100 * millisecond) Tick
              , Keyboard.downs direction
              , Window.resizes SetSize]

-- Display of the game

toCoord : Pos -> (Float, Float)
toCoord (x, y) = (width_of_snake/2 + (toFloat x - (columns/2)) * width_of_snake,
                  width_of_snake/2 + (toFloat y - (rows   /2)) * width_of_snake)

displaySnake : Snake -> Collage.Form
displaySnake { positions, dir } =
    let style = { color = Color.green
                , width = width_of_snake
                , cap   = Collage.Flat
                , join  = Collage.Sharp 10
                , dashOffset = 0
                , dashing    = [] }
    in
      Collage.traced style (Collage.path (List.map toCoord positions))

displayFruit : Pos -> Collage.Form
displayFruit pos =
    let coords = toCoord pos
    in
      Collage.move coords <| Collage.filled Color.red (Collage.rect width_of_snake width_of_snake)

displayWorld : (Int, Int) -> Collage.Form
displayWorld (w, h) = 
    Collage.filled Color.blue (Collage.rect (toFloat w) (toFloat h))

display : Game -> Element.Element
display { snake, fruit, window } =
    if window.width == 0 || fruit == (-1, -1)
    then Element.empty
    else
        Element.container window.width window.height Element.middle 
                   <| Collage.collage width height
                          [ displayWorld (window.width, window.height)
                          , displaySnake snake
                          , displayFruit fruit ]

view : Game -> Html Msg
view game = 
    Html.body [] 
              [Element.toHtml <| display game]


-- Importing main

main =
    Html.program { init = init
                 , view = view
                 , update = update
 , subscriptions = subscriptions }