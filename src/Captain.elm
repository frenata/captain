module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import Array exposing (..)
import Random


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--MODEL


type alias Model =
    { status : Status
    , choice : Maybe Choice
    , action : String
    , since : Maybe Time
    , id : Int
    }


type alias Status =
    { approval : Int
    , food : Int
    , fuel : Int
    , water : Int
    }


newStatus : Status
newStatus =
    { approval = 100, food = 80, fuel = 80, water = 80 }


init : ( Model, Cmd Msg )
init =
    ( { status = newStatus
      , choice = Nothing
      , action = "newgame"
      , since = Nothing
      , id = 58
      }
    , Cmd.none
    )


type alias Choice =
    { question : String
    , right : Answer
    , wrong : Answer
    }


type alias Answer =
    { text : String
    , consequences : List Consequence
    }


type alias Consequence =
    { resource : String
    , change : Int
    }


possibleChoices : Array Choice
possibleChoices =
    Array.fromList
        [ Choice "test"
            (Answer "right"
                [ Consequence "approval" 5
                , Consequence "food" -20
                ]
            )
            (Answer "wrong"
                [ Consequence "approval" -10 ]
            )
        , Choice "test2"
            (Answer "right2"
                [ Consequence "approval" 5 ]
            )
            (Answer "wrong2"
                [ Consequence "approval" -10 ]
            )
        ]



--UPDATE


type Msg
    = Tick Time
    | RightChoice (List Consequence)
    | WrongChoice (List Consequence)
    | CheckApproval
    | Restart
    | Start
    | GetChoice Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( model, Random.generate GetChoice (Random.int 0 ((Array.length possibleChoices) - 1)) )

        GetChoice num ->
            ( { model | choice = Array.get num possibleChoices, action = "choice" }, Cmd.none )

        RightChoice consequences ->
            ( { model | status = applyConsequences consequences model.status, choice = Nothing, action = "playing" }, Cmd.none )

        WrongChoice consequences ->
            { model | status = applyConsequences consequences model.status, choice = Nothing, action = "playing" }
                |> update CheckApproval

        CheckApproval ->
            ( { model
                | action =
                    if model.status.approval < 50 then
                        "gameover"
                    else
                        model.action
              }
            , Cmd.none
            )

        Restart ->
            ( { model | status = newStatus, action = "newgame", id = model.id + 1 }, Cmd.none )

        Start ->
            ( { model | action = "playing" }, Cmd.none )


applyConsequences : List Consequence -> Status -> Status
applyConsequences consequences status =
    List.foldr (updateStatus) status consequences


updateStatus : Consequence -> Status -> Status
updateStatus consequence status =
    let
        resource =
            consequence.resource

        change =
            consequence.change
    in
        case resource of
            "approval" ->
                if withinLimits (status.approval + change) then
                    { status | approval = status.approval + change }
                else
                    status

            "food" ->
                if withinLimits (status.food + change) then
                    { status | food = status.food + change }
                else
                    status

            "fuel" ->
                if withinLimits (status.fuel + change) then
                    { status | fuel = status.fuel + change }
                else
                    status

            "water" ->
                if withinLimits (status.water + change) then
                    { status | water = status.water + change }
                else
                    status

            _ ->
                status


withinLimits : Int -> Bool
withinLimits num =
    num >= 0 && num <= 100



--VIEW


view : Model -> Html Msg
view model =
    case model.action of
        "newgame" ->
            viewWelcome model

        "gameover" ->
            viewGameOver model

        _ ->
            viewGame model


viewGame : Model -> Html Msg
viewGame model =
    div []
        [ div [ class "tracking" ]
            [ ul []
                [ li [] [ text ("Approval: " ++ (toString model.status.approval)) ]
                , li [] [ text ("Food: " ++ (toString model.status.food)) ]
                , li [] [ text ("Fuel: " ++ (toString model.status.fuel)) ]
                , li [] [ text ("Water: " ++ (toString model.status.water)) ]
                ]
            ]
        , viewChoice model
        ]


viewChoice : Model -> Html Msg
viewChoice model =
    case model.choice of
        Just choice ->
            div [ class "choices" ]
                [ h4 [] [ text choice.question ]
                , button [ onClick (RightChoice choice.right.consequences) ] [ text choice.right.text ]
                , button [ onClick (WrongChoice choice.wrong.consequences) ] [ text choice.wrong.text ]
                ]

        Nothing ->
            div [] []


viewWelcome : Model -> Html Msg
viewWelcome model =
    div []
        [ h1 [] [ text ("Welcome Captain #" ++ toString model.id) ]
        , h2 [] [ text "to your Voluntary Captaincy training" ]
        , button [ onClick Start ] [ text "Start" ]
        ]


viewGameOver : Model -> Html Msg
viewGameOver model =
    div []
        [ h2 [] [ text "Game Over" ]
        , button [ onClick Restart ] [ text "Restart" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.action == "choice" then
        Sub.none
    else
        Time.every (5 * second) Tick
