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
        [ Choice "A group of passengers is asking for extra food to celebrate a holy day."
            (Answer "Give them the food, it'll boost morale."
                [ Consequence "approval" 5
                , Consequence "food" -10
                ]
            )
            (Answer "We don't have any to spare."
                [ Consequence "approval" -10 ]
            )
        , Choice "An engineer is requesting promotion."
            (Answer "Promotions are granted to those who earn them."
                [ Consequence "approval" 5 ]
            )
            (Answer "Promise to arrange a promotion for him."
                [ Consequence "approval" -10 ]
            )
        ]



--UPDATE


type Msg
    = Tick Time
    | HumanChoice (List Consequence)
    | CheckStatus
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

        HumanChoice consequences ->
            applyConsequences consequences model
                |> update CheckStatus

        CheckStatus ->
            ( { model
                | action =
                    if model.status.approval < 50 then
                        "low-approval"
                    else if model.status.water == 0 then
                        "no-water"
                    else if model.status.food == 0 then
                        "no-food"
                    else if model.status.fuel == 0 then
                        "no-fuel"
                    else
                        model.action
              }
            , Cmd.none
            )

        Restart ->
            ( { model | status = newStatus, action = "newgame", id = model.id + 1 }, Cmd.none )

        Start ->
            ( { model | action = "playing" }, Cmd.none )


applyConsequences : List Consequence -> Model -> Model
applyConsequences consequences model =
    { model
        | status = List.foldr (updateStatus) model.status consequences
        , choice = Nothing
        , action = "playing"
    }


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
                { status | approval = withinLimits status.approval change }

            "food" ->
                { status | food = withinLimits status.food change }

            "fuel" ->
                { status | fuel = withinLimits status.fuel change }

            "water" ->
                { status | water = withinLimits status.water change }

            _ ->
                status


withinLimits : Int -> Int -> Int
withinLimits current change =
    let
        num =
            current + change
    in
        if num >= 0 && num <= 100 then
            num
        else
            current



--VIEW


view : Model -> Html Msg
view model =
    case model.action of
        "newgame" ->
            viewWelcome model

        "low-approval" ->
            viewGameOver model

        "no-water" ->
            viewGameOver model

        "no-food" ->
            viewGameOver model

        "no-fuel" ->
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
                , button [ onClick (HumanChoice choice.right.consequences) ] [ text choice.right.text ]
                , button [ onClick (HumanChoice choice.wrong.consequences) ] [ text choice.wrong.text ]
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
    let
        message =
            case model.action of
                "low-approval" ->
                    "I'm sorry, due to low approval you have been dismissed from your position."

                "no-water" ->
                    "Without adequate water reserves, the citizens of the ship quickly die off. The computer core, lacking any human to make choices for it, eventually goes mad and flies into a sun."

                "no-food" ->
                    "First food riots break out, followed by cannibalism. You are blamed and one of the first to go. Captains continue to be selected from the dwindling population, but how many will be left by the time a new home is reached?"

                "no-fuel" ->
                    "The ship adrift, the crowds rush for any remaining lifeboats. The bitter remaining population breakdown into rival gangs, warring for territory inside the now lifeless ship."

                _ ->
                    "After many long years of always making the right choice, you have died at your post. You will be remembered."
    in
        div []
            [ h2 [] [ text message ]
            , button [ onClick Restart ] [ text "Restart" ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.action of
        "playing" ->
            Time.every (1 * second) Tick

        _ ->
            Sub.none
