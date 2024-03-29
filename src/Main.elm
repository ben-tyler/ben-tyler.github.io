module Main exposing (..)

{-| As you've probably figured out, this is a checkbox example!
The main purpose of this example is to cover the basics of Elm Animator.
You should checkout these places in the code, which are marked so you can search for them.
It looks like a lot, but much of it is only needed once per project!

  - (1) - Instead of a `Bool`, we store an `Animator Bool` in our model.
  - (2) - The `Animator Model`, which is the piece of code that knows how to update your model when a timeline changes.
  - (3) - Start a timeline by using `Animator.init`
  - (4) - Turning out `Timeline` into a subscription using `Animator.toSubscription`.
  - (5) - Updating our model using our animator and the current time.
  - (6) - Starting an animation
  - (7) - Turning our timeline into inline styles.

-}

import Mario exposing (..)
import Animator
import Animator.Inline
import Browser
import Color
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Encode
import Time


{-| (1) - In our model we'd normally store just a `Bool`.
However now we have an `Animator.Timeline Bool`
-}
type alias Model =
    { checked : Animator.Timeline Bool
    , guide : Animator.Timeline String
    }


{-| (2) - The first thing we do is create an `Animator Model`.
It's job is to reach into our model and update our timelines when they need to be updated
Notice you could add any number of timelines to this animator.
**Note:** You likely only need one animator for a given project.
-}
animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            -- we tell the animator how
            -- to get the checked timeline using .checked
            .guide
            -- and we tell the animator how
            -- to update that timeline as well
            (\newChecked model ->
                { model | guide = newChecked }
            )


main =
    Browser.document
        { init =
            \() ->
                -- (3) - How we create our timeline
                ( { checked = Animator.init False
                  , guide = Animator.init "doo"
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                -- (4) - turning out Animator into a subscription
                -- this is where the animator will decide to have a subscription to AnimationFrame or not.
                animator
                    |> Animator.toSubscription Tick model
        }


type Msg
    = Tick Time.Posix
    | Check Bool
    | Guid String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( model
                |> Animator.update newTime animator
              -- (5) - Updating our model using our animator and the current time.
            , Cmd.none
            )

        Guid newGuid ->
            ( { model
                | guide =
                    model.guide
                        |> Animator.go Animator.verySlowly newGuid
              }
            , Cmd.none
            )

        Check newChecked ->
            ( { model
                | checked =
                    -- (6) - Here we're adding a new state to our timeline.
                    model.checked
                        |> Animator.go Animator.slowly newChecked
              }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "ben-tyler"
    , body =
        [ stylesheet
        , video
            [ Attr.id "myVideo"
            , Attr.property "autoplay" (Json.Encode.bool True)
            , Attr.property "loop" (Json.Encode.bool False)
            , Attr.property "muted" (Json.Encode.bool True)
            , Attr.property "playbackRate" (Json.Encode.float 0.5)
            ]
            [ source [ Attr.src "vid.mp4", Attr.type_ "video/mp4" ] [] ]
        , div
            [ Attr.class "root"
            ]
            [ div
                [ Attr.class "viewport"
                ]
                [ viewGuid model.guide

                --viewHugeCheckbox model.checked
                ]
            ]
        ]
    }


viewGuid : Animator.Timeline String -> Html Msg
viewGuid guide =
    div [Animator.Inline.opacity guide <|
                        \state ->
                            if state == "welcome" then
                                Animator.at 0

                            else
                                Animator.at 1] 
        [ text"hi" ]


viewHugeCheckbox : Animator.Timeline Bool -> Html Msg
viewHugeCheckbox checked =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "flex-direction" "column"
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "cursor" "pointer"
            , Events.onClick (Check (not (Animator.current checked)))
            ]
            [ div
                -- (7) - Rendering our timeline as inline styles.
                -- What we're doing here is mapping our timeline states
                -- to what values they should be in the view.
                -- Elm animator then uses these to interpolate where we should be.
                [ Animator.Inline.backgroundColor checked <|
                    \state ->
                        if state then
                            Color.rgb255 255 255 96

                        else
                            Color.white
                , Animator.Inline.borderColor checked <|
                    \state ->
                        if state then
                            Color.rgb255 255 255 96

                        else
                            Color.black
                , Attr.class "checkbox"
                ]
                [ div
                    [ Animator.Inline.opacity checked <|
                        \state ->
                            if state then
                                Animator.at 1

                            else
                                Animator.at 0
                    , Animator.Inline.transform
                        { position = { x = 0, y = 0 }
                        , rotate =
                            Animator.move checked <|
                                \state ->
                                    if state then
                                        Animator.at (turns 0)

                                    else
                                        Animator.at (turns 0.05)
                        , scale =
                            Animator.move checked <|
                                \state ->
                                    if state then
                                        Animator.at 1

                                    else
                                        Animator.at 0.8
                        }
                    ]
                    [ text "👍" ]
                ]
            , div
                [ Attr.style "margin-left" "32px"
                , Attr.style "font-size" "32px"
                , Attr.style "color" "white"
                ]
                [ text "Welcome"
                , p
                    [ Attr.style "font-size" "14px"
                    ]
                    [ text "ben tyler software developer" ]
                ]
            ]
        , div
            [ Animator.Inline.opacity checked <|
                \state ->
                    if state then
                        Animator.at 1

                    else
                        Animator.at 0
            ]
            [ text ""
            , span
                [ Attr.style "display" "inline-block"
                ]
                [ text "" ]
            ]
        ]


stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ text """@import url('https://fonts.googleapis.com/css?family=Roboto&display=swap');
.root {
    width: 100%;
    height: 1000px;
    font-size: 48px;
    user-select: none;
    font-family: 'Roboto', sans-serif;
}
.viewport {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    margin: 10%;
}
.checkbox {
    border-width: 10px;
    border-style: solid;
    color: #000;
    width: 160px;
    height: 160px;
    border-radius: 20px;
    font-size: 100px;
    line-height: 1.0;
    text-align: center;
}
#myVideo {
  position: fixed;
  right: 0;
  bottom: 0;
  min-width: 100%;
  min-height: 100%;
  z-index: -1;
}
"""
        ]
