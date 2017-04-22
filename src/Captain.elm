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
    { approval : Int
    , food : Int
    , fuel : Int
    , water : Int
    , status : String
    }


init : ( Model, Cmd Msg )
init =
    ( { approval = 100
      , food = 80
      , fuel = 80
      , water = 80
      , status = "playing"
      }
    , Cmd.none
    )



--UPDATE


type Msg
    = PromptChoice String
    | RightChoice
    | WrongChoice
    | CheckApproval
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PromptChoice choice ->
            ( model, Cmd.none )

        RightChoice ->
            ( { model | approval = inc model.approval }, Cmd.none )

        WrongChoice ->
            { model | approval = dec model.approval }
                |> update CheckApproval

        CheckApproval ->
            ( { model
                | status =
                    if model.approval < 50 then
                        "gameover"
                    else
                        model.status
              }
            , Cmd.none
            )

        Restart ->
            init


dec : Int -> Int
dec approval =
    if approval >= 10 then
        approval - 10
    else
        approval


inc : Int -> Int
inc approval =
    if approval <= 95 then
        approval + 5
    else
        approval



--VIEW


view : Model -> Html Msg
view model =
    case model.status of
        "playing" ->
            viewGame model

        _ ->
            viewGameOver model


viewGame : Model -> Html Msg
viewGame model =
    div []
        [ div [ class "tracking" ]
            [ ul []
                [ li [] [ text ("Approval: " ++ (toString model.approval)) ]
                , li [] [ text ("Food: " ++ (toString model.food)) ]
                , li [] [ text ("Fuel: " ++ (toString model.fuel)) ]
                , li [] [ text ("Water: " ++ (toString model.water)) ]
                ]
            ]
        , div [ class "choices" ]
            [ button [ onClick RightChoice ] [ text "Right Choice" ]
            , button [ onClick WrongChoice ] [ text "Wrong Choice" ]
            ]
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
