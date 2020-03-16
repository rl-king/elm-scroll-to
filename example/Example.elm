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
    Sub.batch
        [ Sub.map ScrollToMsg (ScrollTo.subscriptions model.scrollTo)
        , Browser.Events.onKeyUp (onKeyUp "Escape" Cancel)
        ]


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
    | ScrollToIdWithOffset String
    | ScrollToTop


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
                ScrollTo.scrollTo id
            )

        ScrollToTop ->
            ( model
            , Cmd.map ScrollToMsg <|
                ScrollTo.scrollToTop
            )

        ScrollToIdWithOffset id ->
            let
                f { viewport } { element } =
                    { from = { x = viewport.x, y = viewport.y }
                    , to = { x = viewport.x, y = Basics.max 0 (element.y - 100) }
                    }
            in
            ( model
            , Cmd.map ScrollToMsg <|
                ScrollTo.scrollToCustom f id
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
                , onClick (ScrollToId "two")
                , style "margin" "2500px auto"
                , style "display" "block"
                ]
                [ text "Scroll to left" ]
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
                , onClick (ScrollToId "five")
                , style "margin" "0 auto"
                , style "display" "block"
                ]
                [ text "Scroll to right" ]
            ]
        ]
