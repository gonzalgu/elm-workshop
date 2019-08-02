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
    | HandleRegisterResp (Result Http.Error String)
    | SetLoginPlayerId String
    | SetLoginPassword String
    | SetregisterPlayerId String
    | SetregisterPassword String
    | SetregisterPasswordAgain String
    | LoginSubmit
    | RegisterSubmit
    | Reset


type alias Model =
    { backendOK : Bool
    , backendError : Maybe String
    , loginPlayerId : String
    , loginPassword : String
    , registerPlayerId : String
    , registerPassword : String
    , registerPasswordAgain : String
    , registerValidationIssues : List String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { backendOK = True
      , backendError = Nothing
      , loginPlayerId = ""
      , loginPassword = ""
      , registerPlayerId = ""
      , registerPassword = ""
      , registerPasswordAgain = ""
      , registerValidationIssues = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Reset ->
            ( { model | backendOK = True, backendError = Nothing }, Cmd.none )

        HandleLoginResp (Ok r) ->
            ( { model | backendOK = True, backendError = Nothing }, Cmd.none )

        HandleLoginResp (Err err) ->
            ( { model | backendError = Just "Backend login failed", backendOK = False }, Cmd.none )

        HandleRegisterResp (Ok r) ->
            ( { model | backendOK = True, backendError = Nothing }, Cmd.none )

        HandleRegisterResp (Err err) ->
            ( { model | backendError = Just "Backend registration failed", backendOK = False }, Cmd.none )

        SetLoginPlayerId pid ->
            ( { model | loginPlayerId = pid }, Cmd.none )

        SetLoginPassword pwd ->
            ( { model | loginPassword = pwd }, Cmd.none )

        SetregisterPlayerId pid ->
            ( { model | registerPlayerId = pid }, Cmd.none )

        SetregisterPassword pwd ->
            ( { model | registerPassword = pwd }, Cmd.none )

        SetregisterPasswordAgain pwd ->
            ( { model | registerPasswordAgain = pwd }, Cmd.none )

        LoginSubmit ->
            ( model, BE.postApiLogin (BE.DbPlayer model.loginPlayerId model.loginPassword) HandleLoginResp )

        RegisterSubmit ->
            case validateDbPlayer model of
                Ok dbPlayer ->
                    ( { model
                        | registerValidationIssues = []
                      }
                    , BE.postApiPlayers dbPlayer HandleRegisterResp
                    )

                Err problems ->
                    ( { model | registerValidationIssues = problems }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


validateDbPlayer : Model -> Result.Result (List String) BE.DbPlayer
validateDbPlayer model =
    if model.registerPassword == model.registerPasswordAgain then
        Ok (BE.DbPlayer model.registerPlayerId model.registerPassword)

    else
        Err [ "Passwords don't match" ]


view : Model -> H.Html Msg
view model =
    case model.backendError of
        Just er ->
            H.div []
                [ H.div [] [ H.text er ]
                , H.div []
                    [ H.button [ HA.class "btn primary", HE.onClick Reset ] [ H.text "Reset" ]
                    ]
                ]

        Nothing ->
            H.div []
                [ H.div [ HA.class "login-box" ]
                    [ H.h1 [] [ H.text "Registration" ]
                    , H.form
                        [ HE.onSubmit RegisterSubmit ]
                        [ H.input
                            [ HA.placeholder "Player Id"
                            , HAA.ariaLabel "Player ID"
                            , HE.onInput SetregisterPlayerId
                            , HA.value model.registerPlayerId
                            ]
                            []
                        , H.input
                            [ HA.placeholder "Password"
                            , HA.type_ "password"
                            , HAA.ariaLabel "Password"
                            , HE.onInput SetregisterPassword
                            , HA.value model.registerPassword
                            ]
                            []
                        , H.input
                            [ HA.placeholder "Password (again)"
                            , HA.type_ "password"
                            , HAA.ariaLabel "Password"
                            , HE.onInput SetregisterPasswordAgain
                            , HA.value model.registerPasswordAgain
                            ]
                            []
                        , H.button
                            [ HA.class "btn primary" ]
                            [ H.text "Register" ]
                        ]
                    ]
                , H.div [ HA.class "login-box" ]
                    [ H.h1 [] [ H.text "Login" ]
                    , H.form
                        [ HE.onSubmit LoginSubmit
                        ]
                        [ H.input
                            [ HA.placeholder "Player Id"
                            , HAA.ariaLabel "Player ID"
                            , HE.onInput SetLoginPlayerId
                            , HA.value model.loginPlayerId
                            ]
                            []
                        , H.input
                            [ HA.placeholder "Password"
                            , HA.type_ "password"
                            , HAA.ariaLabel "Password"
                            , HE.onInput SetLoginPassword
                            , HA.value model.loginPassword
                            ]
                            []
                        , H.button
                            [ HA.class "btn primary" ]
                            [ H.text "Login" ]
                        ]
                    ]
                ]
