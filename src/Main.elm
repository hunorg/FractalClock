module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text exposing (fromString)
import Color exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Math.Tau exposing (..)
import Time


type alias Model =
    Int


type Msg
    = Tick Time.Posix


view : Model -> Html Msg
view model =
    let
        s =
            toFloat (model |> modBy (60 * 1000)) / (60 * 1000)

        m =
            toFloat (model |> modBy (60 * 60 * 1000)) / (60 * 60 * 1000)

        h =
            toFloat (model |> modBy (12 * 60 * 60 * 1000)) / (12 * 60 * 60 * 1000)

        d =
            toFloat (model |> modBy (30 * 24 * 60 * 60 * 1000)) / (30 * 24 * 60 * 60 * 1000)

        rot z =
            ( sin (τ * z), cos (τ * z) )

        ( ( s_x, s_y ), ( ( m_x, m_y ), ( h_x, h_y ) ), ( d_x, d_y ) ) =
            ( rot s, ( rot m, rot h ), rot d )

        f x y r =
            ( x * r * 100, y * r * 100 )

        seg x y r c =
            segment ( 0, 0 ) (f x y r) |> traced (solid 5 (uniform c))

        move x y r time coll =
            coll |> shift (f x y r) |> rotate (τ * -time) |> scale r

        aux n =
            if n > 0 then
                rectangle 400 400
                    |> filled transparent
                    |> impose (seg s_x s_y 1 lightBlue)
                    |> impose (aux (n - 1) |> move s_x s_y 1 s)
                    |> impose (seg m_x m_y (2 / 3) lightYellow)
                    |> impose (aux (n - 1) |> move m_x m_y (2 / 3) m)
                    |> impose (seg h_x h_y (1 / 2) lightGreen)
                    |> impose (aux (n - 1) |> move h_x h_y (1 / 2) h)
                    |> impose (seg d_x d_y (1 / 3) red)
                    |> impose (aux (n - 1) |> move d_x d_y (1 / 3) d)

            else
                empty
    in
    div [ style "width" "5000px", style "height" "5000px", style "background-color" "black" ]
        [ svg (aux 2)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            ( Time.posixToMillis posix, Cmd.none )


main =
    Browser.element
        { init = \() -> ( 0, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> onAnimationFrame Tick
        }