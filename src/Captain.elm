module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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
    , action : String
    , id : Int
    }


type alias Status =
    { approval : Int
    , food : Int
    , fuel : Int
    , water : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { status = newStatus
      , action = "newgame"
      , id = 58
      }
    , Cmd.none
    )


newStatus : Status
newStatus =
    { approval = 100, food = 80, fuel = 80, water = 80 }



--UPDATE


type Msg
    = PromptChoice String
    | RightChoice
    | WrongChoice
    | CheckApproval
    | Restart
    | Start


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PromptChoice choice ->
            ( model, Cmd.none )

        RightChoice ->
            ( { model | status = updateStatus "approval" 5 model.status }, Cmd.none )

        WrongChoice ->
            { model | status = updateStatus "approval" -10 model.status }
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


updateStatus : String -> Int -> Status -> Status
updateStatus key change status =
    case key of
        "approval" ->
            if withinLimits (status.approval + change) then
                { status | approval = status.approval + change }
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
        "playing" ->
            viewGame model

        "newgame" ->
            viewWelcome model

        _ ->
            viewGameOver model


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
        , div [ class "choices" ]
            [ button [ onClick RightChoice ] [ text "Right Choice" ]
            , button [ onClick WrongChoice ] [ text "Wrong Choice" ]
            ]
        ]


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
    Sub.none
