module Main exposing (..)

import Array
import Browser
import Html exposing (Html, a, button, div, h3, p, text, textarea)
import Html.Attributes exposing (attribute, href, id, placeholder, type_)
import Html.Events as Events
import Http
import Random
import Svg exposing (path, svg)
import Svg.Attributes exposing (class, d, fill, viewBox)



-- MODEL


type alias Deck =
    Array.Array ( String, String )


type alias Model =
    { editingQuestion : Bool, editingAnswer : Bool, showDropdown : Bool, freeTextQuestion : String, freeTextAnswer : String, isEditing : Bool, showQuestion : Bool, showAnswer : Bool, questionNumber : Int, deck : Deck }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { editingQuestion = False, editingAnswer = False, showDropdown = False, freeTextQuestion = "", freeTextAnswer = "", isEditing = False, showQuestion = False, showAnswer = False, questionNumber = 0, deck = Array.empty }
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
    | EditQuestion
    | EditAnswer
    | SaveQuestion
    | SaveAnswer
    | ChangeFreeTextQuestion String
    | ChangeFreeTextAnswer String
    | UpdateDeck
    | GetDeckResponse (Result Http.Error String)
    | PutDeckHttp
    | PutDeckResponse (Result Http.Error String)
    | PickRandom
    | ShowDropdown
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

        EditQuestion ->
            let
                ( question, answer ) =
                    getQuestionAnswer model
            in
            ( { model
                | editingQuestion = True
                , freeTextQuestion = question
                , freeTextAnswer =
                    if model.editingAnswer then
                        model.freeTextAnswer

                    else
                        answer
              }
            , Cmd.none
            )

        EditAnswer ->
            let
                ( question, answer ) =
                    getQuestionAnswer model
            in
            ( { model
                | editingAnswer = True
                , freeTextAnswer = answer
                , freeTextQuestion =
                    if model.editingQuestion then
                        model.freeTextQuestion

                    else
                        question
              }
            , Cmd.none
            )

        SaveQuestion ->
            update UpdateDeck { model | editingQuestion = False }

        SaveAnswer ->
            update UpdateDeck { model | editingAnswer = False }

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

        ShowDropdown ->
            ( { model | showDropdown = not model.showDropdown }, Cmd.none )

        ShowQuestion qn ->
            ( { model | questionNumber = qn, showAnswer = False }
            , Cmd.none
            )

        ShowAnswer ->
            ( { model | showAnswer = not model.showAnswer }
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
    div [ Html.Attributes.class "flex w-full flex-col" ]
        [ div [ Html.Attributes.class "preview flex w-full justify-center data-[align=center]:items-center data-[align=end]:items-end data-[align=start]:items-start p-10", attribute "data-align" "center" ]
            [ div [ Html.Attributes.class "flex w-fit items-stretch [&>*]:focus-visible:z-10 [&>*]:focus-visible:relative [&>[data-slot=select-trigger]:not([class*='w-'])]:w-fit [&>input]:flex-1 has-[select[aria-hidden=true]:last-child]:[&>[data-slot=select-trigger]:last-of-type]:rounded-r-md has-[>[data-slot=button-group]]:gap-2 [&>*:not(:first-child)]:rounded-l-none [&>*:not(:first-child)]:border-l-0 [&>*:not(:last-child)]:rounded-r-none", attribute "data-slot" "button-group", attribute "role" "group" ]
                [ div [ Html.Attributes.class "w-fit items-stretch [&>*]:focus-visible:z-10 [&>*]:focus-visible:relative [&>[data-slot=select-trigger]:not([class*='w-'])]:w-fit [&>input]:flex-1 has-[select[aria-hidden=true]:last-child]:[&>[data-slot=select-trigger]:last-of-type]:rounded-r-md has-[>[data-slot=button-group]]:gap-2 [&>*:not(:first-child)]:rounded-l-none [&>*:not(:first-child)]:border-l-0 [&>*:not(:last-child)]:rounded-r-none hidden sm:flex", attribute "data-slot" "button-group", attribute "role" "group" ]
                    [ button [ attribute "aria-label" "Go Back", Html.Attributes.class "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 size-9", attribute "data-slot" "button", Events.onClick GoToQuizView ]
                        [ Svg.svg [ Svg.Attributes.class "lucide lucide-arrow-left", Svg.Attributes.fill "none", attribute "height" "24", attribute "stroke" "currentColor", attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "stroke-width" "2", Svg.Attributes.viewBox "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                            [ Svg.path [ Svg.Attributes.d "m12 19-7-7 7-7" ]
                                []
                            , Svg.path [ Svg.Attributes.d "M19 12H5" ]
                                []
                            ]
                        ]
                    ]
                , div [ Html.Attributes.class "flex w-fit items-stretch [&>*]:focus-visible:z-10 [&>*]:focus-visible:relative [&>[data-slot=select-trigger]:not([class*='w-'])]:w-fit [&>input]:flex-1 has-[select[aria-hidden=true]:last-child]:[&>[data-slot=select-trigger]:last-of-type]:rounded-r-md has-[>[data-slot=button-group]]:gap-2 [&>*:not(:first-child)]:rounded-l-none [&>*:not(:first-child)]:border-l-0 [&>*:not(:last-child)]:rounded-r-none", attribute "data-slot" "button-group", attribute "role" "group" ]
                    [ button [ Html.Attributes.class "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 h-9 px-4 py-2 has-[>svg]:px-3", attribute "data-slot" "button", Events.onClick UpdateDeck ]
                        [ text "Save changes" ]
                    ]
                ]
            ]
        , div [ Html.Attributes.class "preview flex w-full justify-center data-[align=center]:items-center data-[align=end]:items-end data-[align=start]:items-start p-10", attribute "data-align" "center" ]
            [ textarea
                [ class "border-input placeholder:text-muted-foreground focus-visible:border-ring focus-visible:ring-ring/50 aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive dark:bg-input/30 flex field-sizing-content min-h-16 w-full rounded-md border bg-transparent px-3 py-2 text-base shadow-xs transition-[color,box-shadow] outline-none focus-visible:ring-[3px] disabled:cursor-not-allowed disabled:opacity-50 md:text-sm"
                , attribute "data-slot" "textarea"
                , placeholder "Type the question here."
                , Events.onInput ChangeFreeTextQuestion
                , Html.Attributes.value model.freeTextQuestion
                ]
                [ text model.freeTextQuestion ]
            , textarea
                [ class "border-input placeholder:text-muted-foreground focus-visible:border-ring focus-visible:ring-ring/50 aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive dark:bg-input/30 flex field-sizing-content min-h-16 w-full rounded-md border bg-transparent px-3 py-2 text-base shadow-xs transition-[color,box-shadow] outline-none focus-visible:ring-[3px] disabled:cursor-not-allowed disabled:opacity-50 md:text-sm"
                , attribute "data-slot" "textarea"
                , placeholder "Type the answer here."
                , Events.onInput ChangeFreeTextAnswer
                , Html.Attributes.value model.freeTextAnswer
                ]
                [ text model.freeTextAnswer ]
            ]
        ]


quizView : Model -> Html Msg
quizView model =
    let
        ( question, answer ) =
            getQuestionAnswer model

        accordionState =
            if model.showAnswer then
                "open"

            else
                "closed"

        questionInner =
            if model.editingQuestion then
                textarea
                    [ class "border-input placeholder:text-muted-foreground focus-visible:border-ring focus-visible:ring-ring/50 aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive dark:bg-input/30 flex field-sizing-content min-h-16 w-full rounded-md border bg-transparent px-3 py-2 text-base shadow-xs transition-[color,box-shadow] outline-none focus-visible:ring-[3px] disabled:cursor-not-allowed disabled:opacity-50 md:text-sm"
                    , attribute "data-slot" "textarea"
                    , placeholder "Type the question here."
                    , Events.onInput ChangeFreeTextQuestion
                    , Html.Attributes.value model.freeTextQuestion
                    ]
                    [ text model.freeTextQuestion ]

            else
                p [ Html.Attributes.class "text-muted-foreground line-clamp-2 text-sm leading-normal font-normal text-balance [&>a:hover]:text-primary [&>a]:underline [&>a]:underline-offset-4", attribute "data-slot" "item-description" ]
                    [ text question ]

        questionButton =
            if model.editingQuestion then
                button [ Html.Attributes.class "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 h-8 rounded-md gap-1.5 px-3 has-[>svg]:px-2.5", attribute "data-slot" "button", Events.onClick SaveQuestion ]
                    [ text "Save Question" ]

            else
                button [ Html.Attributes.class "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 h-8 rounded-md gap-1.5 px-3 has-[>svg]:px-2.5", attribute "data-slot" "button", Events.onClick EditQuestion ]
                    [ text "Edit Question" ]

        answerInner =
            if model.editingAnswer then
                textarea
                    [ class "border-input placeholder:text-muted-foreground focus-visible:border-ring focus-visible:ring-ring/50 aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive dark:bg-input/30 flex field-sizing-content min-h-16 w-full rounded-md border bg-transparent px-3 py-2 text-base shadow-xs transition-[color,box-shadow] outline-none focus-visible:ring-[3px] disabled:cursor-not-allowed disabled:opacity-50 md:text-sm"
                    , attribute "data-slot" "textarea"
                    , placeholder "Type the question here."
                    , Events.onInput ChangeFreeTextAnswer
                    , Html.Attributes.value model.freeTextAnswer
                    ]
                    [ text model.freeTextAnswer ]

            else
                p [ Html.Attributes.class "text-muted-foreground line-clamp-2 text-sm leading-normal font-normal text-balance [&>a:hover]:text-primary [&>a]:underline [&>a]:underline-offset-4", attribute "data-slot" "item-description" ]
                    [ text answer ]
    in
    div [ Html.Attributes.class "flex w-full flex-col" ]
        [ div [ Html.Attributes.class "preview flex w-full justify-center data-[align=center]:items-center data-[align=end]:items-end data-[align=start]:items-start p-10", attribute "data-align" "center" ]
            [ div [ Html.Attributes.class "flex w-fit items-stretch [&>*]:focus-visible:z-10 [&>*]:focus-visible:relative [&>[data-slot=select-trigger]:not([class*='w-'])]:w-fit [&>input]:flex-1 has-[select[aria-hidden=true]:last-child]:[&>[data-slot=select-trigger]:last-of-type]:rounded-r-md has-[>[data-slot=button-group]]:gap-2 [&>*:not(:first-child)]:rounded-l-none [&>*:not(:first-child)]:border-l-0 [&>*:not(:last-child)]:rounded-r-none", attribute "data-slot" "button-group", attribute "role" "group" ]
                [ div [ Html.Attributes.class "w-fit items-stretch [&>*]:focus-visible:z-10 [&>*]:focus-visible:relative [&>[data-slot=select-trigger]:not([class*='w-'])]:w-fit [&>input]:flex-1 has-[select[aria-hidden=true]:last-child]:[&>[data-slot=select-trigger]:last-of-type]:rounded-r-md has-[>[data-slot=button-group]]:gap-2 [&>*:not(:first-child)]:rounded-l-none [&>*:not(:first-child)]:border-l-0 [&>*:not(:last-child)]:rounded-r-none hidden sm:flex", attribute "data-slot" "button-group", attribute "role" "group" ]
                    [ button [ attribute "aria-label" "Go Back", Html.Attributes.class "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 size-9", attribute "data-slot" "button" ]
                        [ Svg.svg [ Svg.Attributes.class "lucide lucide-arrow-left", Svg.Attributes.fill "none", attribute "height" "24", attribute "stroke" "currentColor", attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "stroke-width" "2", Svg.Attributes.viewBox "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                            [ Svg.path [ Svg.Attributes.d "m12 19-7-7 7-7" ]
                                []
                            , Svg.path [ Svg.Attributes.d "M19 12H5" ]
                                []
                            ]
                        ]
                    ]
                , div [ Html.Attributes.class "flex w-fit items-stretch [&>*]:focus-visible:z-10 [&>*]:focus-visible:relative [&>[data-slot=select-trigger]:not([class*='w-'])]:w-fit [&>input]:flex-1 has-[select[aria-hidden=true]:last-child]:[&>[data-slot=select-trigger]:last-of-type]:rounded-r-md has-[>[data-slot=button-group]]:gap-2 [&>*:not(:first-child)]:rounded-l-none [&>*:not(:first-child)]:border-l-0 [&>*:not(:last-child)]:rounded-r-none", attribute "data-slot" "button-group", attribute "role" "group" ]
                    [ button [ Html.Attributes.class "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 h-9 px-4 py-2 has-[>svg]:px-3", attribute "data-slot" "button" ]
                        [ text "View all cards" ]
                    , button [ Html.Attributes.class "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 h-9 px-4 py-2 has-[>svg]:px-3", attribute "data-slot" "button", Events.onClick PickRandom ]
                        [ text "Next random question" ]
                    ]
                , div [ Html.Attributes.class "flex w-fit items-stretch [&>*]:focus-visible:z-10 [&>*]:focus-visible:relative [&>[data-slot=select-trigger]:not([class*='w-'])]:w-fit [&>input]:flex-1 has-[select[aria-hidden=true]:last-child]:[&>[data-slot=select-trigger]:last-of-type]:rounded-r-md has-[>[data-slot=button-group]]:gap-2 [&>*:not(:first-child)]:rounded-l-none [&>*:not(:first-child)]:border-l-0 [&>*:not(:last-child)]:rounded-r-none", attribute "data-slot" "button-group", attribute "role" "group" ]
                    [ button [ Html.Attributes.class "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded-md text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 h-9 px-4 py-2 has-[>svg]:px-3", attribute "data-slot" "button", Events.onClick GoToEditView ]
                        [ text "Edit card" ]
                    ]
                ]
            ]
        , div [ Html.Attributes.class "preview flex w-full justify-center data-[align=center]:items-center data-[align=end]:items-end data-[align=start]:items-start p-10", attribute "data-align" "center" ]
            [ div
                [ Html.Attributes.class "flex w-full max-w-md flex-col gap-6" ]
                [ div [ Html.Attributes.class "group/item flex items-center border text-sm rounded-md transition-colors [a]:hover:bg-accent/50 [a]:transition-colors duration-100 flex-wrap outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] border-border p-4 gap-4", attribute "data-size" "default", attribute "data-slot" "item", attribute "data-variant" "outline" ]
                    [ div [ Html.Attributes.class "flex flex-1 flex-col gap-1 [&+[data-slot=item-content]]:flex-none", attribute "data-slot" "item-content" ]
                        [ div [ Html.Attributes.class "flex w-fit items-center gap-2 text-sm leading-snug font-medium", attribute "data-slot" "item-title" ]
                            [ text "Question" ]
                        , questionInner
                        ]
                    , div [ Html.Attributes.class "flex items-center gap-2", attribute "data-slot" "item-actions" ]
                        [ questionButton ]
                    ]
                , a [ Html.Attributes.class "group/item flex items-center border text-sm rounded-md transition-colors [a]:hover:bg-accent/50 [a]:transition-colors duration-100 flex-wrap outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] border-border py-3 px-4 gap-2.5", attribute "data-size" "sm", attribute "data-slot" "item", attribute "data-variant" "outline", href "#" ]
                    [ div [ class "w-full border-b last:border-b-0", attribute "data-orientation" "vertical", attribute "data-slot" "accordion-item", attribute "data-state" accordionState ]
                        [ h3
                            [ class "flex"
                            , attribute "data-orientation" "vertical"
                            , attribute "data-state"
                                accordionState
                            ]
                            [ button
                                [ attribute "aria-controls" "radix-_r_5b_"
                                , attribute "aria-expanded"
                                    (if model.showAnswer then
                                        "true"

                                     else
                                        "false"
                                    )
                                , class "focus-visible:border-ring focus-visible:ring-ring/50 flex flex-1 items-start justify-between gap-4 rounded-md py-4 text-left text-sm font-medium transition-all outline-none hover:underline focus-visible:ring-[3px] disabled:pointer-events-none disabled:opacity-50 [&[data-state=open]>svg]:rotate-180"
                                , attribute "data-orientation" "vertical"
                                , attribute "data-radix-collection-item" ""
                                , attribute "data-slot" "accordion-trigger"
                                , attribute "data-state" accordionState
                                , id "radix-_r_5a_"
                                , type_ "button"
                                , Events.onClick ShowAnswer
                                ]
                                [ text "Answer"
                                , svg [ class "lucide lucide-chevron-down text-muted-foreground pointer-events-none size-4 shrink-0 translate-y-0.5 transition-transform duration-200", fill "none", attribute "height" "24", attribute "stroke" "currentColor", attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "stroke-width" "2", viewBox "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                                    [ path [ d "m6 9 6 6 6-6" ]
                                        []
                                    ]
                                ]
                            ]
                        , div
                            ([ attribute "aria-labelledby" "radix-_r_5a_", class "data-[state=closed]:animate-accordion-up data-[state=open]:animate-accordion-down overflow-hidden text-sm", attribute "data-orientation" "vertical", attribute "data-slot" "accordion-content", attribute "data-state" accordionState, id "radix-_r_5b_", attribute "role" "region", attribute "style" "--radix-accordion-content-height: var(--radix-collapsible-content-height); --radix-accordion-content-width: var(--radix-collapsible-content-width); --radix-collapsible-content-height: 132px; --radix-collapsible-content-width: 472.3666687011719px;" ]
                                ++ (if model.showAnswer then
                                        []

                                    else
                                        [ attribute "hidden" "" ]
                                   )
                            )
                            [ div [ class "pt-0 pb-4 flex flex-col gap-4 text-balance" ]
                                [ p []
                                    [ text answer ]
                                ]
                            ]
                        ]
                    , div [ Html.Attributes.class "flex items-center gap-2", attribute "data-slot" "item-actions" ]
                        [ button [ Html.Attributes.class "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all disabled:pointer-events-none disabled:opacity-50 [&_svg]:pointer-events-none [&_svg:not([class*='size-'])]:size-4 shrink-0 [&_svg]:shrink-0 outline-none focus-visible:border-ring focus-visible:ring-ring/50 focus-visible:ring-[3px] aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 aria-invalid:border-destructive border bg-background shadow-xs hover:bg-accent hover:text-accent-foreground dark:bg-input/30 dark:border-input dark:hover:bg-input/50 h-8 rounded-md gap-1.5 px-3 has-[>svg]:px-2.5", attribute "data-slot" "button", Events.onClick EditAnswer ]
                            [ text "Edit answer" ]
                        ]
                    ]
                ]
            ]
        ]
