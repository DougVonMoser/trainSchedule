module Main exposing (main)

import Browser
import Html exposing (Html, div, text, span)
import Html.Attributes exposing (class, style)
import Task
import Time exposing (Posix, Zone)
import Utils exposing (flip, resetTime)


type alias Model =
    { now : Posix
    , zone : Zone
    , lastMidnight : Posix
    , defaultDestination : Destination
    , schedule : List Train
    }


initialModel : Model
initialModel =
    { now = Time.millisToPosix 0
    , zone = Time.utc
    , lastMidnight = Time.millisToPosix 0
    , defaultDestination = Ogilvie
    , schedule = []
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
                defaultDest =
                    if Time.toHour model.zone model.now > 11 then
                        Palatine
                    else
                        Ogilvie

                zoneOffset =
                    resetTime model.now
                        |> Time.toHour model.zone
                        |> flip (-) 24

                lastUserZoneMidnight =
                    Time.posixToMillis posix
                        + (zoneOffset * 3600000)
                        |> Time.millisToPosix
                        |> resetTime

                schedule =
                    createSchedule lastUserZoneMidnight zoneOffset
            in
                ( { model
                    | now = posix
                    , lastMidnight = lastUserZoneMidnight
                    , defaultDestination = defaultDest
                    , schedule = schedule
                  }
                , Cmd.none
                )

        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )


getTimeTitle : List (Zone -> Posix -> Int) -> Zone -> Posix -> String
getTimeTitle conversionFList zone posix =
    conversionFList
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


findEligbleTrains : Zone -> Posix -> List Train -> Destination -> List (Html Msg)
findEligbleTrains zone now schedule defaultDest =
    let
        subtractNowFrom x =
            ((Time.posixToMillis x - Time.posixToMillis now) + (60 * 1000))
    in
        List.filter (\(Train dest _ _) -> dest == defaultDest) schedule
            |> List.map (\(Train _ dep _) -> dep)
            |> List.filter (subtractNowFrom >> (<) 0)
            |> List.map
                (\x ->
                    ( getTimeTitle [ Time.toHour, Time.toMinute ] zone x
                    , subtractNowFrom x
                        |> flip (//) (60 * 1000)
                    )
                )
            |> List.map trainView


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "top" ]
            [ text (getTimeTitle [ Time.toHour, Time.toMinute, Time.toSecond ] model.zone model.now)
            ]
        , div [ class "rest" ] (findEligbleTrains model.zone model.now model.schedule model.defaultDestination)
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
            |> List.map (String.toInt >> Maybe.withDefault 0)
            |> List.indexedMap
                (\n y ->
                    if n == 0 then
                        y * 3600000
                    else
                        y * 60000
                )
            |> List.foldl (+) 0
            |> (\int ->
                    if isPM then
                        int + (12 * 3600000)
                    else
                        int
               )
            |> (+) (Time.posixToMillis lastMidnight)
            |> (\x -> x + (-offset * 3600000))
            |> Time.millisToPosix


createSchedule : Posix -> Int -> List Train
createSchedule lastMidnight offset =
    [ { destination = Ogilvie, departingTime = "6:49AM", arrivingTime = "7:41AM" }
    , { destination = Ogilvie, departingTime = "6:57AM", arrivingTime = "7:55AM" }
    , { destination = Ogilvie, departingTime = "7:20AM", arrivingTime = "8:01AM" }
    , { destination = Ogilvie, departingTime = "7:24AM", arrivingTime = "8:26AM" }
    , { destination = Ogilvie, departingTime = "7:51AM", arrivingTime = "8:36AM" }
    , { destination = Ogilvie, departingTime = "8:25AM", arrivingTime = "9:25AM" }
    , { destination = Ogilvie, departingTime = "9:25AM", arrivingTime = "10:25AM" }
    , { destination = Palatine, departingTime = "4:39PM", arrivingTime = "5:31PM" }
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
