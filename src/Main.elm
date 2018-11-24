module Main exposing (..)

import Browser
import Html exposing (Html, div, text, span)
import Html.Attributes exposing (class, style)
import Task
import Time exposing (Posix, Zone)


type alias Model =
    { now : Posix
    , zone : Zone
    , lastMidnight : Posix
    , zoneOffset : Int
    }


initialModel : Model
initialModel =
    { now = Time.millisToPosix 0
    , zone = Time.utc
    , lastMidnight = Time.millisToPosix 0
    , zoneOffset = 6
    }


type Destination
    = Ogilvie
    | Palatine


type Train
    = Train Destination Posix Posix


type Msg
    = GotTime Posix
    | GotZone Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTime posix ->
            let
                millis =
                    Time.posixToMillis posix

                lastMidnight =
                    millis - (modBy 86400000 millis) |> Time.millisToPosix

                zoneOffset =
                    (24 - (Time.toHour model.zone lastMidnight))
            in
                ( { model | now = posix, lastMidnight = lastMidnight, zoneOffset = zoneOffset }, Cmd.none )

        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )


getTimeTitle : List (Zone -> Posix -> Int) -> Zone -> Posix -> String
getTimeTitle conversionList zone posix =
    conversionList
        |> List.map (\f -> f zone posix |> String.fromInt |> String.padLeft 2 '0')
        |> String.join ":"


findClosenessColor : Int -> String
findClosenessColor num =
    if num < 5 then
        "red"
    else if num < 10 then
        "yellow"
    else
        "green"


trainView : ( String, Int ) -> Html Msg
trainView ( displayTime, tillNext ) =
    let
        color =
            findClosenessColor tillNext
    in
        div [ class "train", style "border-color" color ]
            [ span [] [ text displayTime ]
            , span [] [ text (" " ++ String.fromInt tillNext ++ " mins") ]
            ]


findEligbleTrains : Zone -> Posix -> List Train -> List (Html Msg)
findEligbleTrains zone posix schedule =
    let
        subtractPosix x y =
            ((Time.posixToMillis x - Time.posixToMillis y) + (60 * 1000))
    in
        List.filter (\(Train dest dep arr) -> dest == Ogilvie) schedule
            |> List.map (\(Train dest dep arr) -> dep)
            |> List.filter (\x -> (<) 0 (subtractPosix x posix))
            |> List.map
                (\x ->
                    ( getTimeTitle [ Time.toHour, Time.toMinute ] zone x
                    , Time.toMinute zone
                        (Time.millisToPosix
                            (subtractPosix x posix)
                        )
                    )
                )
            |> List.map trainView


view : Model -> Html Msg
view model =
    let
        defaultDestination =
            Palatine

        schedule =
            createSchedule model.lastMidnight model.zoneOffset
    in
        div [ class "container" ]
            [ div [ class "top" ]
                [ text (getTimeTitle [ Time.toHour, Time.toMinute, Time.toSecond ] model.zone model.now)
                ]
            , div [ class "rest" ] (findEligbleTrains model.zone model.now schedule)
            ]


main =
    Browser.element
        { init =
            \() ->
                ( initialModel
                , [ Task.perform GotZone Time.here
                  ]
                    |> Cmd.batch
                )
        , subscriptions = \_ -> Time.every 200 GotTime
        , view = view
        , update = update
        }


type AMPM
    = AM
    | PM



-- convert local "3:14PM" to Posix


stringToPosix : Posix -> Int -> String -> Posix
stringToPosix lastMidnight offset string =
    let
        isPM =
            String.contains "PM" string
    in
        string
            |> String.dropRight 2
            |> String.split ":"
            |> List.map String.toInt
            |> List.indexedMap
                (\n y ->
                    if n == 0 then
                        (Maybe.withDefault 1 y * 3600000)
                    else
                        (Maybe.withDefault 1 y * 60000)
                )
            |> List.foldl (+) 0
            |> (\int ->
                    if isPM then
                        int + (12 * 3600000)
                    else
                        int
               )
            |> (+) (Time.posixToMillis lastMidnight)
            |> (+) (offset * 3600000)
            |> Time.millisToPosix


createSchedule : Posix -> Int -> List Train
createSchedule lastMidnight offset =
    [ { destination = Ogilvie, departingTime = "6:49AM", arrivingTime = "7:41AM" }
    , { destination = Ogilvie, departingTime = "6:57AM", arrivingTime = "7:55AM" }
    , { destination = Ogilvie, departingTime = "7:20AM", arrivingTime = "8:01AM" }
    , { destination = Ogilvie, departingTime = "7:24AM", arrivingTime = "8:26AM" }
    , { destination = Ogilvie, departingTime = "7:51AM", arrivingTime = "8:36AM" }
    , { destination = Palatine, departingTime = "4:57PM", arrivingTime = "5:39PM" }
    , { destination = Palatine, departingTime = "5:16PM", arrivingTime = "5:55PM" }
    ]
        |> List.map
            (\x ->
                let
                    createPosix =
                        stringToPosix lastMidnight offset
                in
                    Train x.destination
                        (createPosix x.departingTime)
                        (createPosix x.arrivingTime)
            )
