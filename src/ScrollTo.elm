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
    , scrollToCustom
    , scrollToCustomNoElement
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


# Scroll to custom

@docs scrollToCustom
@docs scrollToCustomNoElement

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
    | SetTarget (Result Browser.Dom.Error { from : Float, to : Float })


{-| Update the `State` with messages sent by `subscriptions` or `Browser.Dom`
viewport information requests.
-}
update : Msg -> State -> ( State, Cmd Msg )
update msg ((State spring) as state) =
    case msg of
        NoOp ->
            ( state, Cmd.none )

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

        SetTarget (Ok { from, to }) ->
            ( State <|
                Spring.setTarget to <|
                    Spring.jumpTo from spring
            , Cmd.none
            )

        SetTarget (Err _) ->
            ( state, Cmd.none )


{-| Scroll to element with given `String` id on the current page.
-}
scrollTo : String -> Cmd Msg
scrollTo id =
    let
        f { viewport } { element } =
            { from = viewport.y
            , to = max 0 element.y
            }
    in
    scrollToCustom f id


{-| Scroll to element with given `String` id on the current page.
-}
scrollToTop : Cmd Msg
scrollToTop =
    let
        f { viewport } =
            { from = viewport.y
            , to = 0
            }
    in
    scrollToCustomNoElement f


{-| Scroll to element with given `String` id on the current page
with your own calculations on where to start and where to end.

For example you could define scroll to with offset like:

    scrollToWithOffset : Float -> String -> Cmd Msg
    scrollToWithOffset offset id =
        let
            f { viewport } { element } =
                { from = viewport.y -- the current position
                , to = max 0 (element.y - offset) -- the element position
                }
        in
        scrollToCustom f id

-}
scrollToCustom :
    (Browser.Dom.Viewport -> Browser.Dom.Element -> { from : Float, to : Float })
    -> String
    -> Cmd Msg
scrollToCustom f id =
    Task.attempt SetTarget <|
        Task.map2 f Browser.Dom.getViewport (Browser.Dom.getElement id)


{-| Scroll to element with given `String` id on the current page
with your own calculations on where to start and where to end.

For example `scrollToTop` is defined like:

    scrollToTop : Cmd Msg
    scrollToTop =
        let
            f { viewport } =
                { from = viewport.y -- the current position
                , to = 0 -- the top of the browser
                }
        in
        scrollToCustomNoElement f

-}
scrollToCustomNoElement :
    (Browser.Dom.Viewport -> { from : Float, to : Float })
    -> Cmd Msg
scrollToCustomNoElement f =
    Task.attempt SetTarget <|
        Task.map f Browser.Dom.getViewport


{-| Interrupt the current animation.

You can call this if the user scrolls during the animation, interrupting
the movement of the page.

-}
cancel : State -> State
cancel (State spring) =
    State (Spring.jumpTo (Spring.target spring) spring)
