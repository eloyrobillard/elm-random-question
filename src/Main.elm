module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array
import Browser
import Html exposing (Html, br, button, div, text)
import Html.Events exposing (onClick)
import Http
import Random



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { showQuestion : Bool, showAnswer : Bool, questionNumber : Int, deck : Array.Array ( String, String ) }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { showQuestion = False, showAnswer = False, questionNumber = 0, deck = Array.empty }
    , getDeckHttp
    )



-- UPDATE


type Msg
    = GetDeckResponse (Result Http.Error String)
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

        PickRandom ->
            ( model
            , Random.generate ShowQuestion (Random.int 0 (Array.length data - 1))
            )

        ShowQuestion qn ->
            ( { model | questionNumber = qn, showAnswer = False }
            , Cmd.none
            )

        ShowAnswer ->
            ( { model | showAnswer = True }
            , Cmd.none
            )


getDeckHttp : Cmd Msg
getDeckHttp =
    Http.get { url = "http://127.0.0.1:8001", expect = Http.expectString GetDeckResponse }


csvToArray : String -> Array.Array ( String, String )
csvToArray csv =
    csv
        |> String.split "\n"
        |> List.map splitStringToTuple
        |> Array.fromList


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
    div []
        [ button [ onClick PickRandom ] [ text "ランダムに選べ" ]
        , button [ onClick ShowAnswer ] [ text "答えを表示" ]
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



-- DATA


data : Array.Array ( String, String )
data =
    Array.fromList
        [ ( "仕事において一番大切なことは何であると思いますか。", "仕事において一番大切なのは学び続けることだと思います。特にエンジニアがなおざりにするコミュニケーション能力がとても大事だと考えます。そのため、私は毎日執筆を行うことで自分のコミュニケーション能力の上昇に励むほか、ソフトスキルに関する本も毎日読んでおります。" )
        , ( "あなたはチームワークを大切にしながらでうまく働いていくことができますか。", "もちろんです。私はよく他のメンバーのコードをレビューしたり、自分のコードに対する感想や助言を求めたりします。現職は在宅仕事になっておりますので、チームへの所属感を高めるためにチームのみなさんと頻繁にビデオ会議を行うことがいかに大事かもわかっております。" )
        , ( "将来的には、国に帰る予定ですか。", "国に両親がおりますので、50代位には日本にはいないかもしれません。ただ、私の目標は日本に関わる仕事をすることなので、少なくとも若いうちは日本でしっかり基盤を作り、将来的には日本の方々向けのサービスを提供していきたいと思います。" )
        , ( "日本で働きたいと考える理由は何ですか。", "日本で働きたい理由は、京都で一年半留学していた間に日本での生活を心から楽しみ、その時から一途に日本に帰りたいと思って参りました。" )
        , ( "プレッシャーをどう克服して仕事の目標を達成していきますか。", "まず先輩と同僚と相談し、中間目標を見直したり、他のエンバー・チームから援助を求めたりするほか、とりあえず毎日仕事を続ければいずれプレッシャーを乗り越えられるでしょう。" )
        , ( "あなたの１年後の目標は何でしょうか。", "本プロジェクトに対する課題をほぼなんでもこなせるようになりたいです。" )
        , ( "あなたの１０年後の目標は何でしょうか。", "起業することです。そのため、10年の間、技術、リーダーシップ、マネジメント能力を伸ばしていきたいと思います。" )
        , ( "これまで直面した困難な事は何でしたか。", "私が担当するはサイバーセキュリティプロジェクトなのにも関わらず入社時に脆弱性は大量にあり、それらをどうすれば解決にできるのか悩んでいました。ほんとんど古いバージョンの問題でしたが、プロジェクトへの影響を最小限に抑えられる上で脆弱性を解決してくれる更新バージョンを探しだすには非常に長い時間かかっていました。そこで、自動的に脆弱性への解決方法を認識するスクリプトを実装し、緊急脆弱性を全て排除し、プロジェクトは今までで一番安全な状態になりました。" )
        , ( "ご自身の強みは何ですか？", "私の強みは忍耐力です。現在担当しているプロジェクトは20万行に及ぶ上に内容も特に複雑であり、初めのところは貢献が難しい状況でした。しかし、簡単なバグ修正などの小規模な課題に集中することで、コードにおけるパターンをつかみ、その大量のコードを効率よく理解していくことができました。また、自分の忍耐力をさらに伸ばすよう、９０万行のオープンソースプロジェクトに毎日触れる習慣を身につけました。" )
        , ( "ご自身の弱みはなんですか？", "私の短所は諦めが悪い所です。現職で、優先度を無視して一つの課題に没頭したあまり、進行せず何日も費やしたことがあります。この経験から、まず課題の優先順位を考え直し、優先度が低ければタイマーなどを使って時間を無駄にしないようにしています。" )
        , ( "過去のプロジェクトでの難しかった課題と、どのように解決しましたか？", "過去にDOS攻撃などの脆弱性解決を担当し、最初に試した修正方法が失敗し、状況が悪化しました。この問題がこれ以上お客さんに及ばないよう、別の方法を考えてコードを一から書き直し、３ヵ月かけて完成しました。この経験からデザイン段階を急がないことの重要さを学びました。" )
        , ( "現職の会社で課題（改善のチャンス）だと思っていることは何ですか？", "私が考えるIBMの改善のチャンスはIBMが理念としている「成長思考」の普及という点にあると考えます。まず「成長思考」ですが、失敗を学びの過程そのものと捉えるべきだという意味を持つ言葉であり、実験がいかに大事なのかを強調する概念の一つです。ただし、自らIBMの理念を調べるまでその言葉を聞いたことはありませんでした。そこで、より積極的に成長思考を私のような社員に教えれば、他のチームとの協調性が高まるでしょう。" )
        , ( "仕事を進めているうえで、気を付けていることはありますか？", "大規模なプロジェクトに携わっていますので、ちょっとした仕様変更やミスで大事になリエますので、コードを書く場合に、変化を最小限に抑えコメントを残し他のチームメンバーに共有する事を心掛けています。" )
        , ( "退職を考えた背景はなんですか？", "IBMに入社する前から日本企業で働きたかったが、コロナの影響で断念しました。おかげさまで、今は幾分収まってきましたので仕事探しを再開しました。" )
        , ( "希望年収はどの程度ですか？", "実はそれについて質問がございます。求人に書いてあった年収は確かに650万円 ~ 1,500万円であり、それがITエンジニアの平均年収（500万以下）を大きく上回ると思います。そこで、具体的にどの役割を求められますか？" )
        , ( "最近気になったニュースは何ですか？", "弊社では、Web開発でよく使われるJavaScript言語の大規模なプロジェクト向け改善版であるTypeScriptを使っております。しかし、Svelteという有名なフレームワークの開発チームは最近、逆にJavaScriptに戻るつもりだと宣言しました。その後に起きた大議論を見て、私たちチームは果たしてTypeScriptに未来はあるかどうか多少心配しています。" )
        , ( "いつ頃から入社できそうですか？", "御社からの在留資格認定証明書をいただいてから２ヶ月以内を目途に入社できるよう、調整させていただきます。詳しくは、条件面談などでご相談させていただけると大変幸いです。" )
        , ( "自己紹介をお願いします。", "本日はお忙しいところ、面接のお時間をいただき、誠にありがとうございます。ロビヤール・エロアと申します。一年半前より、IBMでフロントエンド開発を担当させてまいりました。TypeScriptによるWebアプリケーション開発と保守の経験という観点で、私が持つ知識を活かして御社に貢献することができると考えとります。どうぞよろしくお願いいたします。" )
        , ( "休日はどんなことをして過ごしていますか？", "休日の時間は主に技術能力やキャリアー発展のための学習に注ぎます。" )
        ]
