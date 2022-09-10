module Mario exposing (..)

import Animator
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Json.Encode


type Mario
    = Mario Action Direction


type Action
    = Running
    | Walking
    | Standing
    | Ducking
    | Jumping


type Direction
    = Left
    | Right


animateMario model =
    Animator.step model.mario <|
        \(Mario action direction) ->
            let
                -- this is where we decide to show the left or the right sprite.
                --
                frame mySprite =
                    case direction of
                        Left ->
                            Animator.frame mySprite

                        Right ->
                            Animator.frame { mySprite | flipX = True }
            in
            case action of
                -- for these first three states, we only have a single frame we care about.
                Standing ->
                    frame sprite.tail.stand

                Ducking ->
                    frame sprite.tail.duck

                Jumping ->
                    frame sprite.tail.jump

                Walking ->
                    -- when we're in a `Walking` state, we want to cycle through 3 frames.
                    -- And we can also specify our frames per secton
                    Animator.framesWith
                        -- `transition` are the frames we'd want to take when transitioning to this state.
                        { transition = frame sprite.tail.stand

                        -- `resting` is what we want to do while we're in this state.
                        , resting =
                            Animator.cycle
                                (Animator.fps 15)
                                [ frame sprite.tail.step1
                                , frame sprite.tail.step2
                                , frame sprite.tail.stand
                                ]
                        }

                Running ->
                    -- In order to make mario go faster, we're upping the fps
                    -- and we're also changing the frames so that he puts his arms out.
                    Animator.framesWith
                        { transition = frame sprite.tail.standArms
                        , resting =
                            Animator.cycle
                                (Animator.fps 30)
                                [ frame sprite.tail.runStep1
                                , frame sprite.tail.runStep2
                                , frame sprite.tail.standArms
                                ]
                        }


sprite =
    { tail =
        { stand =
            { x = 0
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 4
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , step1 =
            { x = 30
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , step2 =
            { x = 60
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 4
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , jump =
            { x = 90
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 4
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , duck =
            { x = 120
            , y = 235
            , width = 27
            , height = 30
            , adjustX = 5
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , pivot =
            { x = 150
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 0
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , kick =
            { x = 180
            , y = 240
            , width = 27
            , height = 30
            , adjustX = -1
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , bum =
            { x = 208
            , y = 239
            , width = 20
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , standArms =
            { x = 0
            , y = 280
            , width = 25
            , height = 30
            , adjustX = 4
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , runStep1 =
            { x = 25
            , y = 280
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , runStep2 =
            { x = 52
            , y = 280
            , width = 25
            , height = 30
            , adjustX = 4
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , runJump1 =
            { x = 329
            , y = 280
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , runJump2 =
            { x = 359
            , y = 280
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , runJump3 =
            { x = 389
            , y = 280
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , fall1 =
            { x = 268
            , y = 280
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , fall2 =
            { x = 300
            , y = 280
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        }
    }



type alias Box =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , adjustX : Int
    , adjustY : Int
    , flipX : Bool
    , flipY : Bool
    }


viewSprite : Box -> Html msg
viewSprite box =
    Html.div []
        [ Html.div
            [ Attr.style "position" "absolute"
            , Attr.style "top" (String.fromInt box.adjustY ++ "px")
            , Attr.style "left" (String.fromInt box.adjustX ++ "px")
            , Attr.style "width" (String.fromInt box.width ++ "px")
            , Attr.style "height" (String.fromInt box.height ++ "px")
            , Attr.style "background-image" "url('http://mdgriffith.github.io/elm-animator/images/mario-sprites.png')"
            , Attr.style "background-repeat" "no-repeat"
            , Attr.style "transform-origin" "30% 50%"
            , Attr.style "transform"
                (if box.flipX then
                    "scaleX(-1) scale(2)"

                 else
                    "scaleX(1) scale(2)"
                )
            , Attr.style "background-position"
                ("-"
                    ++ (String.fromInt box.x ++ "px -")
                    ++ (String.fromInt box.y ++ "px")
                )

            -- we need to tell the browser to render our image and leave the pixels pixelated.
            , Attr.class "pixel-art"
            ]
            []
        ]



decodeButton : Decode.Decoder Button
decodeButton =
    Decode.andThen toButton
        (Decode.field "key" Decode.string)


toButton : String -> Decode.Decoder Button
toButton string =
    case string of
        "ArrowLeft" ->
            Decode.succeed GoLeft

        "ArrowRight" ->
            Decode.succeed GoRight

        " " ->
            Decode.succeed Jump

        "ArrowDown" ->
            Decode.succeed Duck

        "Shift" ->
            Decode.succeed Run

        _ ->
            Decode.fail "Skip"




holdButtonsOnGamepad dt gamepad =
    { left = hold dt gamepad.left
    , right = hold dt gamepad.right
    , jump = hold dt gamepad.jump
    , run = hold dt gamepad.run
    , duck = hold dt gamepad.duck
    }


hold dt pressed =
    case pressed of
        NotPressed ->
            NotPressed

        StartPressed ->
            HeldFor dt

        HeldFor existingDt ->
            HeldFor (existingDt + dt)


type alias Milliseconds =
    Float



type Pressed
    = NotPressed
    | StartPressed
    | HeldFor Milliseconds


type alias GamePad =
    { left : Pressed
    , right : Pressed
    , jump : Pressed
    , run : Pressed
    , duck : Pressed
    }

type Button
    = GoLeft
    | GoRight
    | Duck
    | Jump
    | Run

applyButtonToGamepad : Button -> Bool -> GamePad -> GamePad
applyButtonToGamepad button pressed gamepad =
    case button of
        GoLeft ->
            { gamepad
                | left =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        GoRight ->
            { gamepad
                | right =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Duck ->
            { gamepad
                | duck =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Jump ->
            { gamepad
                | jump =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Run ->
            { gamepad
                | run =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

