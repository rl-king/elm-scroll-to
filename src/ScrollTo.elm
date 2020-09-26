module ScrollTo exposing
    ( State
    , Settings
    , init
    , initWithSettings
    , Msg
    , update
    , scrollTo
    , scrollToTop
    , cancel
    , isScrolling
    , scrollToCustom
    , scrollToCustomNoElement
    )

{-| Smoothly scroll to an element or position
on the page with a [spring](https://en.wikipedia.org/wiki/Hooke's_law) animation.


# Init

@docs State
@docs Settings

@docs init
@docs initWithSettings


# Update

@docs Msg
@docs update
@docs subscriptions


# Scroll to

@docs scrollTo
@docs scrollToTop
@docs cancel
@docs isScrolling


# Scroll to custom

@docs scrollToCustom
@docs scrollToCustomNoElement

-}

import Browser.Dom
import Browser.Events
import Spring exposing (Spring)
import Task exposing (Task)
import Time


{-| A type containing all information to get us to our destination during an animation.
-}
type State
    = State Springs


type alias Springs =
    { x : Spring
    , y : Spring
    }


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

These settings are handled by [tad-lispy/springs](https://package.elm-lang.org/packages/tad-lispy/springs/1.0.5/)
so take a look over there on how they interact and play around with
the `Oscillometer` demo to get a visual preview of specific settings.

-}
initWithSettings : Settings -> State
initWithSettings settings =
    State (Springs (Spring.create settings) (Spring.create settings))


{-| A message type for the `State` to update.
-}
type Msg
    = NoOp
    | Tick Int Time.Posix
    | SetTarget
        (Result Browser.Dom.Error
            ( Time.Posix
            , { from : { x : Float, y : Float }
              , to : { x : Float, y : Float }
              }
            )
        )


{-| Update the `State` with messages sent by `subscriptions` or `Browser.Dom`
viewport information requests.
-}
update : (Msg -> msg) -> Msg -> State -> ( State, Cmd msg )
update lift msg ((State springs) as state) =
    let
        setViewport lastNow next =
            Task.perform lift <|
                Task.map2 (\now _ -> Tick (Time.posixToMillis now - Time.posixToMillis lastNow) now)
                    Time.now
                    (Browser.Dom.setViewport (Spring.value next.x) (Spring.value next.y))
    in
    case msg of
        NoOp ->
            ( state, Cmd.none )

        Tick delta lastNow ->
            let
                next =
                    Springs
                        (Spring.animate (toFloat delta) springs.x)
                        (Spring.animate (toFloat delta) springs.y)

                check current next_ target =
                    round ((current - target) + (next_ - target)) == 0
            in
            if
                check (Spring.value springs.x) (Spring.value next.x) (Spring.target next.x)
                    && check (Spring.value springs.y) (Spring.value next.y) (Spring.target next.y)
            then
                ( State <|
                    Springs
                        (Spring.jumpTo (Spring.target springs.x) springs.x)
                        (Spring.jumpTo (Spring.target springs.y) springs.y)
                , Cmd.none
                )

            else
                ( State next
                , setViewport lastNow next
                )

        SetTarget (Ok ( now, { from, to } )) ->
            let
                setCurrent =
                    if isScrolling state then
                        Spring.setTarget

                    else
                        Spring.jumpTo

                next =
                    Springs
                        (Spring.setTarget to.x (setCurrent from.x springs.x))
                        (Spring.setTarget to.y (setCurrent from.y springs.y))
            in
            ( State next
            , setViewport now next
            )

        SetTarget (Err _) ->
            ( state, Cmd.none )


{-| Scroll to element with given `String` id on the current page.

_note: this will only scroll the viewport y-axis to the element y position.
Use `scrollToCustom` if you want more control over this behavior._

-}
scrollTo : (Msg -> msg) -> String -> Cmd msg
scrollTo lift id =
    let
        f { viewport, scene } { element } =
            { from = { x = viewport.x, y = viewport.y }
            , to =
                { x = viewport.x
                , y = min element.y (scene.height - viewport.height)
                }
            }
    in
    scrollToCustom lift f id


{-| Scroll to the top of the page.

_note: this will only scroll the viewport y-axis to 0, the x-axis position
will remain the same._

-}
scrollToTop : (Msg -> msg) -> Cmd msg
scrollToTop lift =
    let
        f { viewport } =
            { from = { x = viewport.x, y = viewport.y }
            , to = { x = viewport.x, y = 0 }
            }
    in
    scrollToCustomNoElement lift f


{-| Scroll to element with given `String` id on the current page
with your own calculations on where to start and where to end.

Both `Viewport` and `Element` can be found in [elm/browser](http://localhost:8009/packages/elm/browser/1.0.2/Browser-Dom)

For example you could define scroll to with offset like:

    scrollToWithOffset : Float -> String -> Cmd Msg
    scrollToWithOffset offset id =
        let
            f { viewport } { element } =
                { from =
                    { x = viewport.x
                    , y = viewport.y
                    }
                , to =
                    { x = viewport.x
                    , y = Basics.max 0 (element.y - 100)
                    }
                }
        in
        scrollToCustom f id

Or scroll the viewport x-axis to the element x position as well.

    scrollToAlt : String -> Cmd Msg
    scrollToAlt id =
        let
            f { viewport } { element } =
                { from = { x = viewport.x, y = viewport.y }
                , to = { x = element.x, y = element.y }
                }
        in
        scrollToCustom f id

-}
scrollToCustom :
    (Msg -> msg)
    ->
        (Browser.Dom.Viewport
         -> Browser.Dom.Element
         ->
            { from : { x : Float, y : Float }
            , to : { x : Float, y : Float }
            }
        )
    -> String
    -> Cmd msg
scrollToCustom lift f id =
    Task.attempt (lift << SetTarget) <|
        Task.map2 Tuple.pair Time.now <|
            Task.map2 f Browser.Dom.getViewport (Browser.Dom.getElement id)


{-| Scroll wherever you like but without an element.

For example `scrollToTop` is defined like:

    scrollToTop : Cmd Msg
    scrollToTop =
        let
            f { viewport } =
                { from = { x = viewport.x, y = viewport.y }
                , to = { x = viewport.x, y = 0 }
                }
        in
        scrollToCustomNoElement f

-}
scrollToCustomNoElement :
    (Msg -> msg)
    ->
        (Browser.Dom.Viewport
         ->
            { from : { x : Float, y : Float }
            , to : { x : Float, y : Float }
            }
        )
    -> Cmd msg
scrollToCustomNoElement lift f =
    Task.attempt (lift << SetTarget) <|
        Task.map2 Tuple.pair Time.now <|
            Task.map f Browser.Dom.getViewport


{-| Interrupt the current animation.

You can call this if the user scrolls during the animation, interrupting
the movement of the page.

-}
cancel : State -> State
cancel (State springs) =
    State <|
        Springs
            (Spring.jumpTo (Spring.target springs.x) springs.x)
            (Spring.jumpTo (Spring.target springs.y) springs.y)


{-| Check if the scrolling animation is running.
-}
isScrolling : State -> Bool
isScrolling (State springs) =
    not (Spring.atRest springs.x && Spring.atRest springs.y)
