module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array
import Browser
import Html exposing (Html, br, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Random



-- MODEL


type alias Model =
    { showQuestion : Bool, showAnswer : Bool, questionNumber : Int, deck : Array.Array ( String, String ) }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { showQuestion = False, showAnswer = False, questionNumber = 0, deck = Array.empty }
    , getDeckHttp
    )



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- UPDATE


type Msg
    = GetDeckResponse (Result Http.Error String)
    | PutDeck
    | PutDeckResponse (Result Http.Error String)
    | PickRandom
    | ShowQuestion Int
    | ShowAnswer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetDeckResponse res ->
            case res of
                Ok deck_csv ->
                    ( { model | deck = csvToArray deck_csv }, Cmd.none )

                Err err ->
                    ( { model | deck = Array.fromList [ ( httpErrorToText err, "" ) ] }, Cmd.none )

        PutDeck ->
            let
                deck_csv =
                    model.deck |> deckToString
            in
            ( model, putDeckHttp deck_csv )

        PutDeckResponse _ ->
            ( model, Cmd.none )

        PickRandom ->
            ( model
            , Random.generate ShowQuestion (Random.int 0 (Array.length model.deck - 1))
            )

        ShowQuestion qn ->
            ( { model | questionNumber = qn, showAnswer = False }
            , Cmd.none
            )

        ShowAnswer ->
            ( { model | showAnswer = True }
            , Cmd.none
            )


putDeckHttp : String -> Cmd Msg
putDeckHttp deck =
    let
        body =
            deck
                |> Http.stringBody "text/plain"
    in
    Http.request
        { method = "PUT", url = "http://127.0.0.1:8001", body = body, expect = Http.expectString PutDeckResponse, timeout = Nothing, tracker = Nothing, headers = [] }


getDeckHttp : Cmd Msg
getDeckHttp =
    Http.get { url = "http://127.0.0.1:8001", expect = Http.expectString GetDeckResponse }


csvToArray : String -> Array.Array ( String, String )
csvToArray csv =
    csv
        |> String.split "\n"
        |> List.map splitStringToTuple
        |> List.filter (\( f, _ ) -> not (String.isEmpty f))
        |> Array.fromList


deckToString : Array.Array ( String, String ) -> String
deckToString deck =
    Array.foldr
        (\( f, s ) acc ->
            String.concat
                [ String.concat
                    [ f
                    , ","
                    , s
                    ]
                , acc
                ]
        )
        ""
        deck


splitStringToTuple : String -> ( String, String )
splitStringToTuple s =
    let
        split =
            String.split "," s

        first =
            Maybe.withDefault "" (List.head split)

        second =
            Maybe.withDefault "" (List.head (Maybe.withDefault [] (List.tail split)))
    in
    ( first, second )


httpErrorToText : Http.Error -> String
httpErrorToText err =
    case err of
        Http.BadUrl _ ->
            "BadUrl"

        Http.BadStatus _ ->
            "BadStatus"

        Http.BadBody _ ->
            "BadBody"

        Http.NetworkError ->
            "NetworkError"

        Http.Timeout ->
            "Timeout"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        ( question, answer ) =
            case Array.get model.questionNumber model.deck of
                Just r ->
                    r

                Nothing ->
                    ( "No question at index " ++ String.fromInt model.questionNumber, "" )
    in
    div [ style "padding" "1rem" ]
        [ button [ onClick PickRandom ] [ text "ランダムに選べ" ]
        , button [ onClick ShowAnswer ] [ text "回答を表示" ]
        , button [ onClick PutDeck ] [ text "デッキを保存" ]
        , br [] []
        , div [] [ text (String.fromInt model.questionNumber) ]
        , br [] []
        , div [] [ text question ]
        , br [] []
        , div []
            [ text
                (if model.showAnswer then
                    answer

                 else
                    ""
                )
            ]
        ]
