module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array
import Browser
import Html exposing (Html, br, button, div, text)
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Http
import Random



-- MODEL


type alias Deck =
    Array.Array ( String, String )


type alias Model =
    { freeTextQuestion : String, freeTextAnswer : String, isEditing : Bool, showQuestion : Bool, showAnswer : Bool, questionNumber : Int, deck : Deck }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { freeTextQuestion = "", freeTextAnswer = "", isEditing = False, showQuestion = False, showAnswer = False, questionNumber = 0, deck = Array.empty }
    , getDeckHttp
    )



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- UPDATE


type Msg
    = GoToEditView
    | GoToQuizView
    | ChangeFreeTextQuestion String
    | ChangeFreeTextAnswer String
    | UpdateDeck
    | GetDeckResponse (Result Http.Error String)
    | PutDeckHttp
    | PutDeckResponse (Result Http.Error String)
    | PickRandom
    | ShowQuestion Int
    | ShowAnswer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToEditView ->
            let
                ( question, answer ) =
                    getQuestionAnswer model
            in
            ( { model | isEditing = True, freeTextQuestion = question, freeTextAnswer = answer }, Cmd.none )

        GoToQuizView ->
            ( { model | isEditing = False }, Cmd.none )

        ChangeFreeTextQuestion text ->
            ( { model | freeTextQuestion = text }, Cmd.none )

        ChangeFreeTextAnswer text ->
            ( { model | freeTextAnswer = text }, Cmd.none )

        GetDeckResponse res ->
            case res of
                Ok deck_csv ->
                    ( { model | deck = csvToArray deck_csv }, Cmd.none )

                Err err ->
                    ( { model | deck = Array.fromList [ ( httpErrorToText err, "" ) ] }, Cmd.none )

        UpdateDeck ->
            let
                newDeckMaybe =
                    buildNewDeck model
            in
            case newDeckMaybe of
                Just newDeck ->
                    update PutDeckHttp { model | deck = newDeck }

                Nothing ->
                    ( model, Cmd.none )

        PutDeckHttp ->
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


getQuestionAnswer : Model -> ( String, String )
getQuestionAnswer model =
    case Array.get model.questionNumber model.deck of
        Just r ->
            r

        Nothing ->
            ( "", "" )


buildNewDeck : Model -> Maybe.Maybe Deck
buildNewDeck model =
    let
        ( question, answer ) =
            getQuestionAnswer model

        newDeck =
            Array.set model.questionNumber ( model.freeTextQuestion, model.freeTextAnswer ) model.deck
    in
    if question == model.freeTextQuestion && answer == model.freeTextAnswer then
        Nothing

    else
        Just newDeck


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


csvToArray : String -> Deck
csvToArray csv =
    csv
        |> String.split "\n"
        |> List.map splitStringToTuple
        |> List.filter (\( f, _ ) -> not (String.isEmpty f))
        |> Array.fromList


deckToString : Deck -> String
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
    if model.isEditing then
        editView model

    else
        quizView model


editView : Model -> Html Msg
editView model =
    let
        ( question, answer ) =
            case Array.get model.questionNumber model.deck of
                Just r ->
                    r

                Nothing ->
                    ( "No question at index " ++ String.fromInt model.questionNumber, "" )
    in
    div [ style "padding" "1rem" ]
        [ button [ Events.onClick UpdateDeck ] [ text "保存する" ]
        , button [ Events.onClick GoToQuizView ] [ text "閉じる" ]
        , Html.textarea
            [ Events.onInput ChangeFreeTextQuestion
            , Attributes.value model.freeTextQuestion
            ]
            []
        , Html.textarea
            [ Events.onInput ChangeFreeTextAnswer
            , Attributes.value model.freeTextAnswer
            ]
            []
        ]


quizView : Model -> Html Msg
quizView model =
    let
        ( question, answer ) =
            getQuestionAnswer model
    in
    div [ style "padding" "1rem" ]
        [ button [ Events.onClick PickRandom ] [ text "ランダムに選べ" ]
        , button [ Events.onClick ShowAnswer ] [ text "回答を表示" ]
        , button [ Events.onClick GoToEditView ] [ text "編集する" ]
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
