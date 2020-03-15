module ScrollTo exposing
    ( State
    , Settings
    , init
    , initWithSettings
    , Msg
    , update
    , subscriptions
    , scrollTo
    , cancel
    )

{-| Smoothly scroll to an element with a [spring](https://en.wikipedia.org/wiki/Hooke's_law) animation.


# Init

@docs State
@docs Settings

@docs init
@docs initWithSettings


# Update

@docs Msg
@docs update
@docs subscriptions


# Scroll to element

@docs scrollTo
@docs cancel

-}

import Browser.Dom
import Browser.Events
import Spring exposing (Spring)
import Task exposing (Task)


{-| A type containing all information to get us to the element during an animation.
-}
type State
    = State Spring


{-| Settings that control how we animate to the element.
-}
type alias Settings =
    { strength : Float
    , dampness : Float
    }


{-| Initial `State` to store in your `Model`.

Settings used are:

    { strength = 100
    , dampness = 4.5
    }

-}
init : State
init =
    initWithSettings (Settings 100 4.5)


{-| Same as `init` but bring your own `Settings`.
-}
initWithSettings : Settings -> State
initWithSettings =
    State << Spring.create


{-| Sync to the browser refresh rate and make sure
our animation runs as smooth as possible.
-}
subscriptions : State -> Sub Msg
subscriptions (State spring) =
    if Spring.atRest spring then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta Tick


{-| A message type for the `State` to update.
-}
type Msg
    = NoOp
    | Tick Float
    | GotViewport (Result Browser.Dom.Error ( Browser.Dom.Viewport, Browser.Dom.Element ))


{-| Update the `State` with messages sent by `subscriptions` or `Browser.Dom`
viewport information requests.
-}
update : Msg -> State -> ( State, Cmd Msg )
update msg ((State spring) as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick delta ->
            let
                next =
                    Spring.animate delta spring

                check current next_ target =
                    round ((current - target) + (next_ - target)) == 0
            in
            if check (Spring.value spring) (Spring.value next) (Spring.target next) then
                ( State (Spring.jumpTo (Spring.target spring) spring)
                , Cmd.none
                )

            else
                ( State next
                , Task.perform (\_ -> NoOp) <|
                    Browser.Dom.setViewport 0 (Spring.value next)
                )

        GotViewport (Ok ( { viewport }, { element } )) ->
            ( State <|
                Spring.setTarget element.y <|
                    Spring.jumpTo viewport.y spring
            , Cmd.none
            )

        GotViewport (Err _) ->
            ( model, Cmd.none )


{-| Scroll to element with given `String` id on the current page.
-}
scrollTo : String -> State -> ( State, Cmd Msg )
scrollTo id model =
    ( model
    , Task.attempt GotViewport <|
        Task.map2 Tuple.pair Browser.Dom.getViewport (Browser.Dom.getElement id)
    )


{-| Interrupt the current animation.

You can call this if the user scrolls during the animation, interrupting
the movement of the page.

-}
cancel : State -> State
cancel (State spring) =
    State (Spring.jumpTo (Spring.target spring) spring)
