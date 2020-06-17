module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Parser exposing ((|.), (|=), Parser)



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


gymCodeToName : Dict String String
gymCodeToName =
    Dict.fromList
        [ ( "ARC", "Arcadia" )
        , ( "ERV", "East Riverside" )
        , ( "HDS", "High Desert" )
        , ( "LON", "Long Beach" )
        , ( "MVJ", "Mission Viejo" )
        , ( "RCU", "Rancho Cucamonga" )
        , ( "RIV", "Riverside" )
        , ( "SCL", "San Clemente" )
        , ( "SPB", "South Bay" )
        , ( "UPL", "Upland" )
        ]


type alias GymInfo =
    { capacity : Int
    , occupancy : Int
    }


type Model
    = Failed
    | Loaded (Dict String GymInfo)
    | Loading


init : () -> ( Model, Cmd Msg )
init () =
    ( Loading, fetchGymInfo )


fetchGymInfo : Cmd Msg
fetchGymInfo =
    Http.get
        { url = "https://cors-anywhere.herokuapp.com/https://portal.rockgympro.com/portal/public/74083a89f418928244e5479ea18be366/occupancy"
        , expect = Http.expectString NewFetch
        }



-- UPDATE


type Msg
    = NewFetch (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewFetch result ->
            case result of
                Ok data ->
                    case Parser.run parseHtml data of
                        Ok gymInfoDict ->
                            ( Loaded gymInfoDict, Cmd.none )

                        Err _ ->
                            ( Failed, Cmd.none )

                Err _ ->
                    ( Failed, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


black : Color
black =
    rgb255 10 10 10


white : Color
white =
    rgb255 225 225 225


blue : Color
blue =
    rgb255 21 101 192


gradientArray : Array Color
gradientArray =
    Array.fromList
        [ rgba255 0 299 0 0.5
        , rgba255 48 299 0 0.5
        , rgba255 96 230 0 0.5
        , rgba255 144 231 0 0.5
        , rgba255 193 231 0 0.5
        , rgba255 232 222 0 0.5
        , rgba255 233 174 0 0.5
        , rgba255 233 174 0 0.5
        , rgba255 233 126 0 0.5
        , rgba255 234 78 0 0.5
        , rgba255 235 30 0 0.5
        ]


gradient : Float -> Maybe Color
gradient value =
    let
        clampedValue =
            clamp 0 1 value

        index =
            -- Give every option a roughly equal range
            round <| -0.499 + 9.998 * clampedValue
    in
    Array.get index gradientArray


view : Model -> Browser.Document Msg
view model =
    case model of
        Failed ->
            viewFailed

        Loading ->
            viewLoading

        Loaded gymInfoDict ->
            viewLoaded gymInfoDict


viewFailed : Browser.Document msg
viewFailed =
    { title = "CliffHangar"
    , body =
        [ layout
            [ Background.color black
            , Font.family
                [ Font.external
                    { name = "Work Sans"
                    , url = "https://fonts.googleapis.com/css2?family=Work+Sans&display=swap"
                    }
                , Font.sansSerif
                ]
            , Font.color white
            , Font.center
            , padding 10
            , spacing 7
            ]
          <|
            column []
                [ text "Whoops, something went wrong..."
                , text "Try visiting the official Hangar 18 website:"
                , link []
                    { url = "https://www.climbhangar18.com"
                    , label = text "Hangar 18 official website"
                    }
                ]
        ]
    }


viewLoading : Browser.Document msg
viewLoading =
    { title = "CliffHangar"
    , body =
        [ layout
            [ Background.color black
            , Font.family
                [ Font.external
                    { name = "Work Sans"
                    , url = "https://fonts.googleapis.com/css2?family=Work+Sans&display=swap"
                    }
                , Font.sansSerif
                ]
            , Font.color white
            , Font.center
            , padding 10
            , spacing 7
            ]
          <|
            text "Scraping https://www.climbhangar18.com, please wait..."
        ]
    }


viewLoaded : Dict String GymInfo -> Browser.Document msg
viewLoaded gymInfoDict =
    { title = "CliffHangar"
    , body =
        [ layout
            [ Background.color black
            , Font.family
                [ Font.external
                    { name = "Work Sans"
                    , url = "https://fonts.googleapis.com/css2?family=Work+Sans&display=swap"
                    }
                , Font.sansSerif
                ]
            , Font.color white
            , Font.center
            ]
          <|
            viewGymInfoDict gymInfoDict
        ]
    }


borderWidth =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


viewGymInfoDict : Dict String GymInfo -> Element msg
viewGymInfoDict gymInfoDict =
    column [ width fill, height fill ]
        ([ row
            [ width fill
            , height fill
            , Background.color blue
            , Border.color white
            , Border.widthEach { borderWidth | bottom = 2 }
            ]
            [ el [ width fill ] <| text "Gym"
            , el [ width fill ] <| text "Occupancy"
            , el [ width fill ] <| text "Capacity"
            ]
         ]
            ++ List.map viewGymInfoEntry (Dict.toList gymInfoDict)
        )


viewGymInfoEntry : ( String, GymInfo ) -> Element msg
viewGymInfoEntry ( name, info ) =
    case gradient <| toFloat info.occupancy / toFloat info.capacity of
        Nothing ->
            row
                [ width fill
                , height fill
                , Border.color white
                , Border.widthEach { borderWidth | bottom = 2 }
                ]
                [ el [ width fill ] <| text name
                , el [ width fill ] <| text <| String.fromInt info.occupancy
                , el [ width fill ] <| text <| String.fromInt info.capacity
                ]

        Just gradientColor ->
            row
                [ width fill
                , height fill
                , Border.color white
                , Border.widthEach { borderWidth | bottom = 2 }
                ]
                [ el [ width fill ] <| text name
                , el
                    [ height fill
                    , width fill
                    , Background.color gradientColor
                    , Font.glow black 1
                    ]
                  <|
                    el [ width fill, centerY ] <|
                        text <|
                            String.fromInt info.occupancy
                , el [ width fill ] <| text <| String.fromInt info.capacity
                ]



-- SCRAPE


mapCodeToName : ( String, GymInfo ) -> Maybe ( String, GymInfo )
mapCodeToName ( code, gymInfo ) =
    Dict.get code gymCodeToName
        |> Maybe.map (\name -> ( name, gymInfo ))


mapGymCodes : List ( String, GymInfo ) -> Dict String GymInfo
mapGymCodes gymInfoList =
    List.filterMap mapCodeToName gymInfoList
        |> Dict.fromList


parseHtml : Parser (Dict String GymInfo)
parseHtml =
    Parser.succeed identity
        |. Parser.chompUntil "<script>"
        |. Parser.chompUntil "{"
        |= parseGymInfoList
        |> Parser.map mapGymCodes


parseGymInfoList : Parser (List ( String, GymInfo ))
parseGymInfoList =
    Parser.sequence
        { start = "{"
        , separator = ","
        , end = "}"
        , spaces = Parser.spaces
        , item = parseGymInfoEntry
        , trailing = Parser.Optional
        }


singleQuotedString : Parser String
singleQuotedString =
    Parser.succeed identity
        |. Parser.token "'"
        |= (Parser.getChompedString <| Parser.chompUntil "'")
        |. Parser.token "'"


parseGymInfoDict : Parser (Dict String GymInfo)
parseGymInfoDict =
    Parser.sequence
        { start = "{"
        , separator = ","
        , end = "}"
        , spaces = Parser.spaces
        , item = parseGymInfoEntry
        , trailing = Parser.Optional
        }
        |> Parser.map Dict.fromList


parseDictKey : Parser String
parseDictKey =
    singleQuotedString
        |. Parser.spaces
        |. Parser.token ":"
        |. Parser.spaces


parseGymInfoEntry : Parser ( String, GymInfo )
parseGymInfoEntry =
    Parser.succeed Tuple.pair
        |= parseDictKey
        |= parseGymInfo


parseGymInfo : Parser GymInfo
parseGymInfo =
    Parser.succeed GymInfo
        |. Parser.token "{"
        |. Parser.spaces
        |= parseIntDictEntry "capacity"
        |. Parser.spaces
        |. Parser.token ","
        |. Parser.spaces
        |= parseIntDictEntry "count"
        |. Parser.chompUntil "}"
        |. Parser.token "}"


parseIntDictEntry : String -> Parser Int
parseIntDictEntry key =
    Parser.succeed identity
        |. (parseDictKey |> Parser.andThen (matchesKey key))
        |= Parser.int


matchesKey : String -> String -> Parser String
matchesKey key candidate =
    if key == candidate then
        Parser.succeed key

    else
        Parser.problem <|
            "\""
                ++ candidate
                ++ "\" does not match key \""
                ++ key
                ++ "\""
