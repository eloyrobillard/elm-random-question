module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (attribute, class, id, style)
import Html.Events as Events
import Http
import Random
import Svg
import Svg.Attributes



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
                deckCSV =
                    model.deck |> deckToString
            in
            ( model, putDeckHttp deckCSV )

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
                , "\n"
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
    div [ style "padding" "1rem" ]
        [ button [ Events.onClick UpdateDeck ] [ text "保存する" ]
        , button [ Events.onClick GoToQuizView ] [ text "閉じる" ]
        , div [ style "display" "flex", style "flex-direction" "column" ]
            [ Html.textarea
                [ Events.onInput ChangeFreeTextQuestion
                , Html.Attributes.value model.freeTextQuestion
                ]
                []
            , Html.textarea
                [ Events.onInput ChangeFreeTextAnswer
                , Html.Attributes.value model.freeTextAnswer
                ]
                []
            ]
        ]


quizView : Model -> Html Msg
quizView model =
    let
        ( question, answer ) =
            getQuestionAnswer model
    in
    div [ class "preview flex w-full justify-center data-[align=center]:items-center data-[align=end]:items-end data-[align=start]:items-start h-[450px] p-10", attribute "data-align" "center" ]
        [ div [ class "flex w-fit items-stretch [&>*]:focus-visible:z-10 [&>*]:focus-visible:relative [&>[data-slot=select-trigger]:not([class*='w-'])]:w-fit [&>input]:flex-1 has-[select[aria-hidden=true]:last-child]:[&>[data-slot=select-trigger]:last-of-type]:rounded-r-md has-[>[data-slot=button-group]]:gap-2 [&>*:not(:first-child)]:rounded-l-none [&>*:not(:first-child)]:border-l-0 [&>*:not(:last-child)]:rounded-r-none", attribute "data-slot" "button-group", attribute "role" "group" ]
            [ div [ class "w-fit items-stretch [&>*]:focus-visible:z-10 [&>*]:focus-visible:relative [&>[data-slot=select-trigger]:not([class*='w-'])]:w-fit [&>input]:flex-1 has-[select[aria-hidden=true]:last-child]:[&>[data-slot=select-trigger]:last-of-type]:rounded-r-md has-[>[data-slot=button-group]]:gap-2 [&>*:not(:first-child)]:rounded-l-none [&>*:not(:first-child)]:border-l-0 [&>*:not(:last-child)]:rounded-r-none hidden sm:flex", attribute "data-slot" "button-group", attribute "role" "group" ]
                [ button [ attribute "aria-label" "Go Back", class "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 size-9", attribute "data-slot" "button" ]
                    [ Svg.svg [ Svg.Attributes.class "lucide lucide-arrow-left", Svg.Attributes.fill "none", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "stroke" "currentColor", Html.Attributes.attribute "stroke-linecap" "round", Html.Attributes.attribute "stroke-linejoin" "round", Html.Attributes.attribute "stroke-width" "2", Svg.Attributes.viewBox "0 0 24 24", Html.Attributes.attribute "width" "24", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ]
                        [ Svg.path [ Svg.Attributes.d "m12 19-7-7 7-7" ]
                            []
                        , Svg.path [ Svg.Attributes.d "M19 12H5" ]
                            []
                        ]
                    ]
                ]
            , div [ class "flex w-fit items-stretch [&>*]:focus-visible:z-10 [&>*]:focus-visible:relative [&>[data-slot=select-trigger]:not([class*='w-'])]:w-fit [&>input]:flex-1 has-[select[aria-hidden=true]:last-child]:[&>[data-slot=select-trigger]:last-of-type]:rounded-r-md has-[>[data-slot=button-group]]:gap-2 [&>*:not(:first-child)]:rounded-l-none [&>*:not(:first-child)]:border-l-0 [&>*:not(:last-child)]:rounded-r-none", attribute "data-slot" "button-group", attribute "role" "group" ]
                [ button [ class "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 h-9 px-4 py-2 has-[>svg]:px-3", attribute "data-slot" "button", Events.onClick PickRandom ]
                    [ text "Random question" ]
                , button [ class "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 h-9 px-4 py-2 has-[>svg]:px-3", attribute "data-slot" "button", Events.onClick ShowAnswer ]
                    [ text "Show answer" ]
                ]
            , div [ class "flex w-fit items-stretch [&>*]:focus-visible:z-10 [&>*]:focus-visible:relative [&>[data-slot=select-trigger]:not([class*='w-'])]:w-fit [&>input]:flex-1 has-[select[aria-hidden=true]:last-child]:[&>[data-slot=select-trigger]:last-of-type]:rounded-r-md has-[>[data-slot=button-group]]:gap-2 [&>*:not(:first-child)]:rounded-l-none [&>*:not(:first-child)]:border-l-0 [&>*:not(:last-child)]:rounded-r-none", attribute "data-slot" "button-group", attribute "role" "group" ]
                [ button [ class "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 h-9 px-4 py-2 has-[>svg]:px-3", attribute "data-slot" "button", Events.onClick GoToEditView ]
                    [ text "Edit card" ]
                , button [ attribute "aria-expanded" "false", attribute "aria-haspopup" "menu", attribute "aria-label" "More Options", class "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 size-9", attribute "data-slot" "dropdown-menu-trigger", attribute "data-state" "closed", id "radix-_r_a0_", Svg.Attributes.type_ "button" ]
                    [ Svg.svg
                        [ Svg.Attributes.class "lucide lucide-ellipsis", Svg.Attributes.fill "none", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "stroke" "currentColor", Html.Attributes.attribute "stroke-linecap" "round", Html.Attributes.attribute "stroke-linejoin" "round", Html.Attributes.attribute "stroke-width" "2", Svg.Attributes.viewBox "0 0 24 24", Html.Attributes.attribute "width" "24", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ]
                        [ Svg.node "circle"
                            [ attribute "cx" "12", attribute "cy" "12", attribute "r" "1" ]
                            []
                        , Svg.node "circle"
                            [ attribute "cx" "19", attribute "cy" "12", attribute "r" "1" ]
                            []
                        , Svg.node "circle"
                            [ attribute "cx" "5", attribute "cy" "12", attribute "r" "1" ]
                            []
                        ]
                    ]
                ]
            ]
        ]



-- div [ style "padding" "1rem" ]
--     [ button [ Events.onClick PickRandom ] [ text "ランダムに選べ" ]
--     , button [ Events.onClick ShowAnswer ] [ text "回答を表示" ]
--     , button [ Events.onClick GoToEditView ] [ text "編集する" ]
--     , br [] []
--     , div [] [ text (String.fromInt model.questionNumber) ]
--     , br [] []
--     , div [] [ text question ]
--     , br [] []
--     , div []
--         [ text
--             (if model.showAnswer then
--                 answer
--
--              else
--                 ""
--             )
--         ]
--     ]
