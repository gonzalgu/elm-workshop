module Main exposing (main)

import Browser
import Debug
import Generated.Api as BE
import Html as H
import Html.Attributes as HA
import Html.Attributes.Aria as HAA
import Html.Events as HE
import Http
import RemoteData exposing (RemoteData)
import Session
import Utils


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = HandleLoginResp (Result Http.Error String)
    | SetLoginPlayerId String
    | SetLoginPassword String
    | SetRegisterPlayerId String
    | SetRegisterPassword String
    | SetRegisterPasswordAgain String
    | LoginSubmit
    | RegisterSubmit


type alias Model =
    { backendOK : Bool
    , loginError : Maybe String
    , loginPlayerId : String
    , loginPassword : String
    , loginToken : Maybe String
    , registerPlayerId : String
    , registerPassword : String
    , registerPasswordAgain : String
    , registerToken : Maybe String
    , registerValidationIssues : List String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { backendOK = True
      , loginError = Nothing
      , loginPlayerId = ""
      , loginPassword = ""
      , loginToken = Nothing
      , registerPlayerId = ""
      , registerPassword = ""
      , registerPasswordAgain = ""
      , registerToken = Nothing
      , registerValidationIssues = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        HandleLoginResp (Ok token) ->
            ( { model | backendOK = True, loginError = Nothing, loginToken = Just token }, Cmd.none )

        HandleLoginResp (Err err) ->
            ( { model | loginError = Just "Backend login failed", backendOK = False }, Cmd.none )

        SetLoginPlayerId pid ->
            ( { model | loginPlayerId = pid }, Cmd.none )

        SetLoginPassword pwd ->
            ( { model | loginPassword = pwd }, Cmd.none )

        SetRegisterPlayerId pid ->
            ( { model | registerPlayerId = pid }, Cmd.none )

        SetRegisterPassword pwd ->
            ( { model | registerPassword = pwd }, Cmd.none )

        SetRegisterPasswordAgain pwd ->
            ( { model | registerPasswordAgain = pwd }, Cmd.none )

        LoginSubmit ->
            ( model, BE.postApiLogin (BE.DbPlayer model.loginPlayerId model.loginPassword) HandleLoginResp )

        RegisterSubmit ->
            case validateDbPlayer model of
                Err problems ->
                    ( { model | registerValidationIssues = problems, registerToken = Nothing }, Cmd.none )

                Ok dbPlayer ->
                    ( { model | registerValidationIssues = [], registerToken = Nothing }, BE.postApiPlayers dbPlayer HandleLoginResp )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> H.Html Msg
view model =
    case model.loginError of
        Just err ->
            H.div [] [ H.h1 [] [ H.text "OH NOES" ] ]

        Nothing ->
            H.div []
                [ H.div [ HA.class "login-box" ]
                    [ H.h1 [] [ H.text "Login" ]
                    , H.form [ HE.onSubmit LoginSubmit ]
                        [ H.input
                            [ HA.placeholder "Player Id"
                            , HAA.ariaLabel "Player ID"
                            , HE.onInput SetLoginPlayerId
                            ]
                            []
                        , H.input
                            [ HA.placeholder "Password"
                            , HA.type_ "password"
                            , HAA.ariaLabel "Password"
                            , HE.onInput SetLoginPassword
                            ]
                            []
                        , H.button
                            [ HA.class "btn primary" ]
                            [ H.text "Login" ]
                        ]
                    ]
                , H.div [ HA.class "login-box" ]
                    [ H.h1 [] [ H.text "Register" ]
                    , H.form [ HE.onSubmit RegisterSubmit ]
                        [ H.input
                            [ HA.placeholder "Player Id"
                            , HAA.ariaLabel "Player ID"
                            , HE.onInput SetRegisterPlayerId
                            ]
                            []
                        , H.input
                            [ HA.placeholder "Password"
                            , HA.type_ "password"
                            , HAA.ariaLabel "Password"
                            , HE.onInput SetRegisterPassword
                            ]
                            []
                        , H.input
                            [ HA.placeholder "Password again"
                            , HA.type_ "password"
                            , HAA.ariaLabel "Password again"
                            , HE.onInput SetRegisterPasswordAgain
                            ]
                            []
                        , H.button
                            [ HA.class "btn primary" ]
                            [ H.text "Login" ]
                        ]
                    ]
                ]


validateDbPlayer : Model -> Result.Result (List String) BE.DbPlayer
validateDbPlayer model =
    if model.registerPassword == "" then
        Err [ "password is empty" ]

    else if model.registerPassword /= model.registerPasswordAgain then
        Err [ "passwords do not match" ]

    else
        Ok (BE.DbPlayer model.registerPlayerId model.registerPassword)
