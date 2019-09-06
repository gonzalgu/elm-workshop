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
import Time as Time
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
    | HandleChatMessageResp (Result Http.Error ())
    | SetLoginPlayerId String
    | SetLoginPassword String
    | SetRegisterPlayerId String
    | SetRegisterPassword String
    | SetRegisterPasswordAgain String
    | SetChatMessage String
    | LoginSubmit
    | RegisterSubmit
    | ChatSubmit
    | Tick Session.Player Time.Posix


type alias Model =
    { backendOK : Bool
    , loginPlayerId : String
    , loginPassword : String
    , loginToken : RemoteData String String
    , registerPlayerId : String
    , registerPassword : String
    , registerPasswordAgain : String
    , registerToken : RemoteData String String
    , registerValidationIssues : List String
    , player : Maybe Session.Player
    , chatMessage : String
    , chatLines : List BE.ChatLine
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { backendOK = True
      , loginPlayerId = ""
      , loginPassword = ""
      , loginToken = RemoteData.NotAsked
      , registerPlayerId = ""
      , registerPassword = ""
      , registerPasswordAgain = ""
      , registerToken = RemoteData.NotAsked
      , registerValidationIssues = []
      , player = Nothing
      , chatMessage = ""
      , chatLines = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        HandleLoginResp (Ok token) ->
            ( { model
                | backendOK = True
                , loginToken = RemoteData.Success token
                , player = Just (Session.Player model.loginPlayerId token)
              }
            , Cmd.none
            )

        HandleLoginResp (Err err) ->
            ( { model
                | loginToken = RemoteData.Failure "Backend login failed"
              }
            , Cmd.none
            )

        HandleRegisterResp (Ok token) ->
            ( { model
                | backendOK = True
                , registerToken = RemoteData.Success token
                , player = Just (Session.Player model.registerPlayerId token)
              }
            , Cmd.none
            )

        HandleRegisterResp (Err err) ->
            ( { model | registerToken = RemoteData.Failure (Utils.httpErrorToStr err) }, Cmd.none )

        HandleChatMessageResp (Ok ()) ->
            ( model, Cmd.none )

        HandleChatMessageResp (Err err) ->
            ( model, Cmd.none )

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

        SetChatMessage msg ->
            ( { model | chatMessage = msg }, Cmd.none )

        LoginSubmit ->
            ( model, BE.postApiLogin (BE.DbPlayer model.loginPlayerId model.loginPassword) HandleLoginResp )

        RegisterSubmit ->
            case validateDbPlayer model of
                Err problems ->
                    ( { model | registerValidationIssues = problems, registerToken = RemoteData.NotAsked }, Cmd.none )

                Ok dbPlayer ->
                    ( { model | registerValidationIssues = [], registerToken = RemoteData.Loading }, BE.postApiPlayers dbPlayer HandleRegisterResp )

        ChatSubmit ->
            ( model, chatSubmit model )

        Tick player time ->
            ( model, Cmd.none )


chatSubmit : Model -> Cmd Msg
chatSubmit model =
    case model.player of
        Nothing ->
            Cmd.none

        Just player ->
            BE.postApiLobby player.token model.chatMessage HandleChatMessageResp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.player of
        Nothing ->
            Sub.none

        Just p ->
            Time.every 2000 (Tick p)


view : Model -> H.Html Msg
view model =
    case model.player of
        Nothing ->
            loggedOutView model

        Just p ->
            loggedInView p model


loggedInView : Session.Player -> Model -> H.Html Msg
loggedInView player model =
    -- This is just some boilerplate markup
    H.div [ HA.class "lobby" ]
        [ H.div [ HA.class "lobby-games" ]
            [ H.h1 [] [ H.text "Lobby" ]
            ]
        , H.div [ HA.class "chatbox-container" ]
            [ H.h2 [] [ H.text "Chat Lobby" ]
            , H.div [ HA.id "chatbox", HA.class "chatbox" ] (List.map chatLineView model.chatLines)
            , H.form [ HE.onSubmit ChatSubmit ]
                [ H.ul []
                    [ H.li [ HA.class "chat-message" ]
                        [ H.input
                            [ HA.placeholder "type a chat message"
                            , HA.class "chat-message-input"
                            , HAA.ariaLabel "Enter Chat Message"
                            , HE.onInput SetChatMessage
                            ]
                            []
                        ]
                    , H.li []
                        [ H.button
                            [ HA.class "btn primary" ]
                            [ H.text "send" ]
                        ]
                    ]
                ]
            ]
        ]


loggedOutView : Model -> H.Html Msg
loggedOutView model =
    let
        loading =
            H.div [] [ H.h1 [] [ H.text "Standby" ] ]

        ohno =
            H.div [] [ H.h1 [] [ H.text "OH NOES" ] ]
    in
    H.div []
        [ case model.loginToken of
            RemoteData.Failure err ->
                ohno

            _ ->
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
                    ]
        , case model.registerToken of
            RemoteData.Failure err ->
                ohno

            RemoteData.Loading ->
                loading

            _ ->
                H.div [ HA.class "login-box" ]
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
                            [ H.text "Register" ]
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


chatLineView : BE.ChatLine -> H.Html Msg
chatLineView cl =
    H.div []
        [ H.p [] [ H.text cl.chatLinePlayerId ]
        , H.p [] [ H.text cl.chatLineText ]
        ]
