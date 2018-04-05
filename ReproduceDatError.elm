module ReproduceDatError exposing (main)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http exposing (Error(..), Response)


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, Cmd.none )
        }



--
-- Everything below is copied directly from the Counter.elm example
-- in the Elm Guide.
--


type alias Model =
    { lastResponse : String
    }


model : Model
model =
    { lastResponse = "nothing yet"
    }


type Msg
    = ClickButton
    | ClickButtonResponse (Result Http.Error String)


url : String
url =
    "https://reqres.in/api/users?page=2"


getRequest : Http.Request String
getRequest =
    Http.getString url


putRequest : Http.Request String
putRequest =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\x ->
                    x
                        |> Debug.log "response: "
                        |> .url
                        |> Ok
                )
        , timeout = Nothing
        , withCredentials = False
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickButton ->
            ( model
            , Http.send ClickButtonResponse putRequest
            )

        ClickButtonResponse (Ok url) ->
            ( { model | lastResponse = url }
            , Cmd.none
            )

        ClickButtonResponse (Err err) ->
            ( { model
                | lastResponse =
                    case err of
                        BadUrl string ->
                            "BadUrl :" ++ string

                        Timeout ->
                            "timeout"

                        NetworkError ->
                            "network error"

                        BadStatus response ->
                            "bad status " ++ response.body

                        BadPayload str response ->
                            "bad payload status " ++ response.body
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ Html.span [] [ Html.text model.lastResponse ]
        , button [ onClick ClickButton ] [ text "send ajax request" ]
        ]
