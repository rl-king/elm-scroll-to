# elm-scroll-to

Smoothly scroll to an element or position with a [spring](https://en.wikipedia.org/wiki/Hooke's_law) animation.

[example live](https://rl-king.github.io/elm-scroll-to-example/) |
[example code](https://github.com/rl-king/elm-scroll-to/tree/master/example).

## Add to your `Model`

```elm
type alias Model =
    { scrollTo : ScrollTo.State }


init : ( Model, Cmd Msg )
init =
    ( { scrollTo = ScrollTo.init }
    , Cmd.none
    )

```

## Wire `Msg`s

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ScrollToMsg <|
        ScrollTo.subscriptions model.scrollTo


type Msg
    = ScrollToMsg ScrollTo.Msg
    | ScrollToId String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScrollToMsg scrollToMsg ->
            let
                ( scrollToModel, scrollToCmds ) =
                    ScrollTo.update
                        scrollToMsg
                        model.scrollTo
            in
            ( { model | scrollTo = scrollToModel }
            , Cmd.map ScrollToMsg scrollToCmds
            )

        ScrollToId id ->
            ( model
            , Cmd.map ScrollToMsg <|
                  ScrollTo.scrollTo id model.scrollTo
            )
```

## In your view
```elm
div []
    [ button
        [ id "one"
        , onClick (ScrollToId "two")
        , style "display" "block"
        ]
        [ text "Go 👇" ]
    , button
        [ id "two"
        , onClick (ScrollToId "one")
        , style "margin" "2500px 0"
        , style "display" "block"
        ]
        [ text "Go 👆" ]
    ]
```

## Credits

Made with help of [tad-lispy/springs](https://package.elm-lang.org/packages/tad-lispy/springs/latest/)
and ideas from [linuss/smooth-scroll](https://package.elm-lang.org/packages/linuss/smooth-scroll/latest/).
