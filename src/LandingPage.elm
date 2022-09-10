module LandingPage exposing (..)

import Animator
import Animator.Css
import Animator.Inline
import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Json.Encode
import Keyboard exposing (Key(..))
import Keyboard.Arrows exposing (Arrows)
import Main exposing (viewGuid)
import Mario exposing (..)
import Svg
import Svg.Attributes
import Task
import Time
import Tree exposing (Tree)
import Tree.Build as Build
import Tree.Render
import Tree.Svg
import Tree.Zipper
import LineChart.Events exposing (Event)
import Html.Events


{-|


# Run around as Mario!

This example is primarily here to show how the sprite system works.
Essentially we're going to be animating like you would with a flip book!
But we're also going to be doing some physics calculations directly ourselves, so this example shows how you'd use `elm-animator` side-by-side with other manual animations if you need to.
(1) **Sprite Sheet** - A sprite sheet is single image file with a bunch of images at different positions in the file. [Here's the one we're using](https://github.com/mdgriffith/elm-animator/blob/master/examples/images/mario-sprites.png)
To animate this, we need to pick out our subimages from our image file by knowing their bounding boxes.
(2) **Animating the flipbook** - Here's the new place where we're animating Mario'd state.

-}
{--}
type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , window : Window
    , gamepad : GamePad
    , mario : Animator.Timeline Mario
    , evaler : Animator.Timeline String
    , pressedKeys : List Key
    , dslContent : String
    , dslEditor : DslEditor
    }


type alias DslEditor =
    { level : Int -- number of indentations
    , word : Int -- current editing word
    }


holdButtons : Float -> Model -> Model
holdButtons dt model =
    let
        gamepad =
            model.gamepad
    in
    { model
        | gamepad = holdButtonsOnGamepad dt gamepad
    }


type alias Keys =
    { x : Int
    , y : Int
    }


type Arrow
    = ArrowDown
    | ArrowUp
    | ArrowLeft
    | ArrowRight


type alias Window =
    { width : Int
    , height : Int
    }


type Msg
    = Tick Time.Posix
    | Frame Float
    | Pressed Button
    | Released Button
    | WindowSize Int Int
    | KeyMsg Keyboard.Msg
    | Eval 


main =
    Browser.document
        { init =
            \() ->
                ( init
                , Browser.Dom.getViewport
                    |> Task.attempt
                        (\viewportResult ->
                            case viewportResult of
                                Ok viewport ->
                                    WindowSize
                                        (round viewport.scene.width)
                                        (round viewport.scene.height)

                                Err err ->
                                    WindowSize
                                        (round 800)
                                        (round 600)
                        )
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


test =
    """
this
    is the kind
       of
    structure
       we
       want    """


init : Model
init =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , window = { width = 800, height = 500 }
    , gamepad =
        { left = NotPressed
        , right = NotPressed
        , jump = NotPressed
        , run = NotPressed
        , duck = NotPressed
        }
    , mario = Animator.init (Mario Standing Right)
    , evaler = Animator.init "some guide"
    , pressedKeys = []
    , dslContent = ""
    , dslEditor =
        { level = 0
        , word = 0
        }
    }


letterDelete str =
    str
        |> String.reverse
        |> String.uncons
        |> (\unconsd ->
                case unconsd of
                    Just ( x, xs ) ->
                        xs

                    _ ->
                        ""
           )
        |> String.reverse


wordDelete str =
    str
        |> String.words
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> String.concat


duckValue content level =
    content ++ "\n" ++ String.repeat level """    """ ++ "🦆"


arrowFunc content level =
    content ++ "\n" ++ String.repeat level """    """ ++ "🔀"


monkeyFunction content level =
    let
        indentation =
            String.repeat level """    """

        funcIndentation =
            String.repeat (level - 1) """    """

        lambdaSignature =
            "🐀"
    in
    content
        ++ "\n"
        ++ funcIndentation
        ++ lambdaSignature



--    ++ "\n"
--    ++ indentation
--    ++ ""


handleDsl keyMsg model =
    let
        editorState =
            List.foldl
                (\key result ->
                    case key of
                        Control ->
                            { result | level = result.level + 2 }

                        Keyboard.ArrowUp ->
                            { result | level = result.level - 2 }

                        Keyboard.ArrowDown ->
                            { result | level = result.level + 2 }

                        -- indents too levels, one for function, one for content
                        _ ->
                            result
                )
                model.dslEditor
                (Keyboard.update keyMsg [])

        contentString =
            List.foldl
                (\key content ->
                    case key of
                        Spacebar ->
                            content ++ """ \t"""

                        Keyboard.Enter ->
                            duckValue content editorState.level

                        Keyboard.Backspace ->
                            content
                                |> wordDelete

                        Character c ->
                            content ++ c

                        --Shift ->
                        --    arrowFunc content editorState.level
                        Control ->
                            monkeyFunction content editorState.level

                        _ ->
                            content
                )
                model.dslContent
                (Keyboard.update keyMsg [])

        parseContent content =
            content
    in
    { model
        | dslContent = contentString
        , dslEditor = editorState
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( model
                |> Animator.update newTime animator
            , Cmd.none
            )

        Frame dt ->
            ( model
                |> holdButtons dt
                |> gravity (dt / 10)
                |> jump model.gamepad
                |> walk model.gamepad
                |> physics (dt / 10)
                |> updateSprites
            , Cmd.none
            )

        Pressed button ->
            ( { model
                | gamepad =
                    applyButtonToGamepad button True model.gamepad
              }
            , Cmd.none
            )

        Released button ->
            ( { model
                | gamepad =
                    applyButtonToGamepad button False model.gamepad
              }
            , Cmd.none
            )

        WindowSize width height ->
            ( { model
                | window =
                    { width = width
                    , height = height
                    }
              }
            , Cmd.none
            )

        KeyMsg keyMsg ->
            ( model
                |> handleDsl keyMsg
                |> (\m -> { m | pressedKeys = Keyboard.update keyMsg m.pressedKeys })
            , Cmd.none
            )

        Eval -> 
            ( {model | evaler = 
                model.evaler
                    |> Animator.go Animator.slowly (evalRunner model)}
            , Cmd.none
            )


evalRunner model = 
    let 
        tree =
            Build.fromString "?" .content model.dslContent
    in
    case tree of 
        Result.Ok r ->
            r
            |> Tree.Zipper.fromTree
            |> Tree.Zipper.label

        Result.Err m -> ""

    --(Build.fromString "?" .content model.dslContent)
    --|> Result.withDefault (Tree.tree "!!!")
    --|> Tree.Zipper.fromTree
    --|> Tree.Zipper.label


{-| -}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ animator
            |> Animator.toSubscription Tick model
        , Browser.Events.onResize WindowSize
        , Browser.Events.onKeyDown (Decode.map Pressed decodeButton)
        , Browser.Events.onKeyUp (Decode.map Released decodeButton)
        , Browser.Events.onAnimationFrameDelta Frame
        , Sub.map KeyMsg Keyboard.subscriptions
        ]


animator : Animator.Animator Model
animator =
    Animator.animator
        -- we tell the animator how to get the checked timeline using .checked
        -- and we tell the animator how to update that timeline with updateChecked
        |> Animator.watching .mario (\mario m -> { m | mario = mario })
        |> Animator.watching .evaler (\evaler m -> { m | evaler = evaler })


viewBackgroundVideo =
    Html.video
        [ Attr.id "backgroundVid"
        , Attr.property "autoplay" (Json.Encode.bool True)
        , Attr.property "loop" (Json.Encode.bool False)
        , Attr.property "muted" (Json.Encode.bool True)
        , Attr.property "playbackRate" (Json.Encode.float 0.5)
        ]
        [ Html.source [ Attr.src "vid.mp4", Attr.type_ "video/mp4" ] [] ]


viewEvaler : Animator.Timeline String -> Html Msg
viewEvaler evalerTimeline =
    Html.div
        [ Animator.Inline.opacity evalerTimeline <|
            \evaler ->
                if evaler == "helloworld" then
                    Animator.at 1

                else
                    Animator.at 1
        ]
        [ Html.text <| Animator.current evalerTimeline]


labelToHtml : String -> Html msg
labelToHtml l =
    Html.text <| String.toLower l


toListItems : Html msg -> List (Html msg) -> Html msg
toListItems label children =
    case children of
        [] ->
            Html.li
                []
                [ label ]

        _ ->
            Html.li
                []
                [ label
                , Html.ul [] children
                ]


viewTree model =
    let
        tree =
            Build.fromString "?" .content model.dslContent

        graph =
            Result.map
                (Tree.Render.toGraph
                    { halfAngle = 0.25 * pi
                    , initialEdgeLength = 2
                    , scaleFactor = 1
                    , ballRadius = 3
                    , ballColor = "white"
                    }
                    identity
                )
                tree
                |> Result.withDefault []
    in
    Svg.svg
        [ Svg.Attributes.width "800"
        , Svg.Attributes.height "800"

        --, Svg.Attributes.viewBox ("0 0 " ++ w ++ " " ++ h)
        --   , Svg.Attributes.viewBox "0 0 100% 100%"
        --  , Svg.Attributes.fill "white"
        ]
        ([]
            ++ Tree.Svg.render Tree.Svg.FullLabel (Tree.Svg.transform 280 100 60 60 0.5 graph)
        )


view : Model -> Browser.Document Msg
view model =
    { title = "ben tyler"
    , body =
        [ stylesheet
        , viewBackgroundVideo
        , Html.div
            [ Attr.style "position" "fixed"
            , Attr.style "left" "0"
            , Attr.style "top" "0"
            , Attr.style "width" (String.fromInt model.window.width ++ "px")
            , Attr.style "height" (String.fromInt model.window.height ++ "px")
            ]
            [ Html.div
                [ Attr.class "positioner"
                , Attr.style "position" "absolute"
                , Attr.style "top" (String.fromFloat ((toFloat model.window.height / 9) - model.y) ++ "px")
                , Attr.style "right" (String.fromFloat 100 ++ "px")
                , Attr.style "width" "300px"
                ]
                [ Html.text <| String.toLower <| Debug.toString model.evaler ]
            , Html.div
                [ Attr.style "float" "right"
                ]
                [ viewTree model ]
            , Html.div
                [ Attr.style "position" "absolute"
                , Attr.style "top" "80px"
                , Attr.style "left" "80px"
                , Attr.style "user-select" "none"
                , Attr.style "font-family" "'Roboto', sans-serif"
                ]
                [ Html.h1 [] [ Html.text "welcome" ]
                , Html.div [] [ Html.text "ben tyler - software developer" ]
                , Html.div 
                    [ Attr.style "border-style" "solid" 
                    , Attr.style "padding" "5px"
                    ] 
                    [ Html.h3 [] [Html.text " ducklang {{{ ctr = Rat Func, enter = duck obj }}}"]
                    , Html.hr [] []
                    , viewText model 
                    , Html.button [ Html.Events.onClick Eval] [Html.text ">>>"]
                    , Html.hr [] [] 
                    , viewEvaler model.evaler
                    ]

                --, Html.div []
                --    [ Html.text <|
                --        String.toLower <|
                --            Debug.toString <|
                --                Build.fromString "?" .content model.dslContent
                --    ]
                --, Html.div []
                --    [ Html.text <|
                --        String.toLower <|
                --            Debug.toString <|
                --                model.dslContent
                --    ]
                ]
            , Html.div
                [ Attr.class "positioner"
                , Attr.style "position" "absolute"
                , Attr.style "top" (String.fromFloat ((toFloat model.window.height / 1.067) - model.y) ++ "px")
                , Attr.style "left" (String.fromFloat model.x ++ "px")
                ]
                -- (2) - Animating Mario's state with sprites
                --      We're watching te model.mario timeline, which has both a direction and an action that mario is currently doing.
                --
                [ viewSprite <| animateMario model
                ]
            ]
        ]
    }


viewText model =
    Build.fromString "?" .content model.dslContent
        |> Result.toMaybe
        |> (\maybe ->
                case maybe of
                    Just r ->
                        r
                            |> Tree.restructure labelToHtml toListItems
                            |> (\root ->
                                    Html.ul
                                        [-- Attr.style "border-style" "solid"
                                         --  , Attr.style "padding" "4%"
                                        ]
                                        [ --Html.text "dsl context"
                                          root
                                        ]
                               )

                    Nothing ->
                        Html.ul [] []
           )


isHeld : Pressed -> Bool
isHeld pressed =
    case pressed of
        NotPressed ->
            False

        StartPressed ->
            True

        HeldFor _ ->
            True


walk : GamePad -> Model -> Model
walk pad mario =
    let
        run yes x =
            if yes then
                x * 2.0

            else
                x

        newVx =
            if isHeld pad.left && isHeld pad.right then
                0

            else if isHeld pad.left then
                run (isHeld pad.run) -1.8

            else if isHeld pad.right then
                run (isHeld pad.run) 1.8

            else
                0
    in
    { mario
        | vx = newVx
    }


jump : GamePad -> Model -> Model
jump pad mario =
    if pad.jump == StartPressed && mario.vy == 0 then
        { mario | vy = 6.0 }

    else
        mario


gravity : Float -> Model -> Model
gravity dt mario =
    { mario
        | vy =
            if mario.y > 0 then
                mario.vy - dt / 4

            else
                0
    }


physics : Float -> Model -> Model
physics dt mario =
    { mario
        | x =
            (mario.x + dt * mario.vx)
                |> min (toFloat mario.window.width - 40)
                |> max 0
        , y = max 0 (mario.y + dt * mario.vy)
    }


updateSprites : Model -> Model
updateSprites model =
    let
        current =
            Animator.current model.mario

        direction =
            if model.vx > 0 then
                Right

            else if model.vx < 0 then
                Left

            else
                case current of
                    Mario _ currentDirection ->
                        currentDirection

        action =
            if model.y /= 0 then
                Jumping

            else if model.vx /= 0 then
                if abs model.vx > 2 then
                    Running

                else
                    Walking

            else if isHeld model.gamepad.duck then
                Ducking

            else
                Standing

        newMario =
            Mario action direction
    in
    if current /= newMario then
        { model
            | mario =
                model.mario
                    |> Animator.go Animator.immediately newMario
        }

    else
        model



{- (1) - Sprite Sheet
   x, y -> the coordinates of the image on the sprite sheet
   width, height -> the size of the image I want
   adjustX, adjustY -> adjustX and adjustY move the position of the rendered image so that we can line it up with the previous frames.
   flipX, flipY ->  The sprite sheet only shows mario looking in one direction.  Though we can flip that image if we need to!
-}


stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ Html.text """@import url('https://fonts.googleapis.com/css?family=Roboto&display=swap');
body, html {
    margin: 0;
    padding:0;
    border:0;
    display:block;
    position: relative;
    width: 100%;
    height: 100%;
    color: white;
}
.pixel-art {
    image-rendering: pixelated;
    image-rendering: -moz-crisp-edges;
    image-rendering: crisp-edges;
}
#backgroundVid {
  position: fixed;
  right: 0;
  bottom: 0;
  min-width: 100%;
  min-height: 100%;
  z-index: -1;
}
li {
    list-style-type: none;
}
"""
        ]
