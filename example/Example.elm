module Example exposing (main)

import Browser exposing (Document)
import Browser.Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import ScrollTo
import Task exposing (Task)



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyUp (onKeyUp "Escape" Cancel)


onKeyUp : String -> msg -> Decode.Decoder msg
onKeyUp key msg =
    let
        toMsg pressedKey =
            if key == pressedKey then
                Decode.succeed msg

            else
                Decode.fail "Unknown key"
    in
    Decode.andThen toMsg <|
        Decode.field "key" Decode.string



-- MODEL


type alias Model =
    { scrollTo : ScrollTo.State }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { scrollTo = ScrollTo.init }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Cancel
    | ScrollToMsg ScrollTo.Msg
    | ScrollToId String
    | ScrollToIdAlt String
    | ScrollToIdWithOffset String
    | ScrollToTop
    | ResetScroll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Cancel ->
            ( { model | scrollTo = ScrollTo.cancel model.scrollTo }
            , Cmd.none
            )

        ScrollToMsg scrollToMsg ->
            let
                ( scrollToModel, scrollToCmds ) =
                    ScrollTo.update ScrollToMsg scrollToMsg model.scrollTo
            in
            ( { model | scrollTo = scrollToModel }
            , scrollToCmds
            )

        ScrollToId id ->
            ( model
            , ScrollTo.scrollTo ScrollToMsg id
            )

        ScrollToIdAlt id ->
            let
                f { viewport } { element } =
                    { from = { x = viewport.x, y = viewport.y }
                    , to =
                        { x = element.x - viewport.width / 2
                        , y = element.y - viewport.height / 2
                        }
                    }
            in
            ( model
            , ScrollTo.scrollToCustom ScrollToMsg f id
            )

        ScrollToTop ->
            ( model
            , ScrollTo.scrollToTop ScrollToMsg
            )

        ScrollToIdWithOffset id ->
            let
                f { viewport } { element } =
                    { from = { x = viewport.x, y = viewport.y }
                    , to = { x = viewport.x, y = Basics.max 0 (element.y - 100) }
                    }
            in
            ( model
            , ScrollTo.scrollToCustom ScrollToMsg f id
            )

        ResetScroll ->
            let
                f { viewport } =
                    { from = { x = viewport.x, y = viewport.y }
                    , to = { x = 0, y = 0 }
                    }
            in
            ( model
            , ScrollTo.scrollToCustomNoElement ScrollToMsg f
            )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "elm-scroll-to"
    , body = [ viewButtons ]
    }


viewButtons : Html Msg
viewButtons =
    div []
        [ button
            [ onClick (ScrollToId "two")
            , style "margin" "20px auto"
            , style "display" "block"
            ]
            [ text "Scroll to next button" ]
        , div [ style "display" "flex", style "width" "200vw" ]
            [ button
                [ id "two"
                , onClick (ScrollToIdWithOffset "three")
                , style "margin" "2500px auto"
                , style "display" "block"
                ]
                [ text "Scroll to next button with offset" ]
            , button
                [ id "five"
                , onClick ResetScroll
                , style "margin" "2500px auto"
                , style "display" "block"
                ]
                [ text "Reset" ]
            ]
        , div [ style "margin" "2500px 0" ]
            [ button
                [ id "three"
                , onClick ScrollToTop
                , style "margin" "0 auto"
                , style "display" "block"
                ]
                [ text "Back to top" ]
            , button
                [ id "four"
                , onClick (ScrollToIdAlt "five")
                , style "margin" "0 auto"
                , style "display" "block"
                ]
                [ text "Scroll to right" ]
            ]
        ]
