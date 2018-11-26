port module Home exposing (..)

import TypeAlias exposing (TypeAlias)
import TypeAlias.O18
import UnionType
import Types
import String
import Util
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Css exposing (..)
-- import Css.Elements as Css
-- import Css.Namespace exposing (namespace)
-- import Html.CssHelpers
import ColorScheme exposing (..)
import Json.Decode
import Json.Encode as Json


type CssClasses
    = Content
    | Input
    | Output
    | Alias


-- css : Stylesheet
-- css =
--     (stylesheet << namespace cssNamespace)
--         [ Css.body
--             [ backgroundColor accent1 ]
--         , (.) Content
--             [ Css.width (px 960)
--             , margin2 zero auto
--             ]
--         , each [ (.) Input, (.) Output ]
--             [ Css.width (pct 40)
--             , Css.height (px 500)
--             , fontFamily monospace
--             ]
--         , aliasCss
--         ]


onChange : msg -> Attribute msg
onChange msg =
    Html.Events.on "change" (Json.Decode.succeed msg)


viewAllAliases : ElmVersion -> String -> DecoderType -> List TypeAlias -> Html Action
viewAllAliases version incoming decoder aliases =
    let
        formattedAliases =
            List.map TypeAlias.aliasFormat aliases

        decoderCreator =
            TypeAlias.O18.createPipelineDecoder

        imports =
            TypeAlias.O18.pipelineImports

        encoder =
            TypeAlias.O18.createEncoder

        extra =
            []

        output =
            [ [ imports ]
            , formattedAliases
            , List.map decoderCreator formattedAliases
            , List.map encoder formattedAliases
            , extra
            ]
                |> List.concat
                |> String.join "\n\n"
    in
        Html.textarea
            [ value <| output
            -- , class [ Output ]
            , style "height" "100%"
            , style "width" "40%"
            ]
            []


viewTypeAliasStuff : ElmVersion -> String -> Html Action
viewTypeAliasStuff version incoming =
    let
        decoder =
            TypeAlias.O18.createDecoder incoming

        encoder =
            TypeAlias.O18.createEncoder incoming

        output =
            [ decoder
            , encoder
            ]
                |> String.join "\n\n"
    in
        Html.textarea
            [ value <| output
            -- , class [ Output ]
            , spellcheck False
            ]
            []


-- viewAllUnions : String -> Html Action
-- viewAllUnions union =
--     let
--         type_ =
--             UnionType.createUnionType union

--         output =
--             [ UnionType.createTypeFromString type_
--             , UnionType.createDecoder type_
--             , UnionType.createEncoder type_
--             ]
--                 |> String.join "\n\n"
--     in
--         Html.textarea
--             [ value <| output
--             , class [ Output ]
--             , spellcheck False
--             ]
--             []


viewInput : String -> Html Action
viewInput alias =
    Html.textarea
        [ onInput UpdateInput
        -- , class [ Input ]
        , style "height" "100%"
        , style "width" "40%"
        , spellcheck False
        , placeholder "Put a valid JSON object in here!"
        -- , placeholder "Put a valid JSON object in here! Now try a type alias, an union type, or an old-style decoder!"
        ]
        [ Html.text <| alias ]


-- radio : (a -> Action) -> a -> a -> String -> Html Action
-- radio onUpdate selected decoder name =
--     span []
--         [ input
--             [ type_ "radio"
--             , Html.Attributes.checked (selected == decoder)
--             , onChange (onUpdate decoder)
--             ]
--             []
--         , Html.text name
--         ]


-- viewDecoderTypeInput : DecoderType -> Html Action
-- viewDecoderTypeInput decoder =
--     div
--         []
--         [ Html.text "Decoder type: "
--         , radio UpdateDecoder decoder Original "original"
--         , radio UpdateDecoder decoder Pipeline "pipeline"
--         , radio UpdateDecoder decoder English "English"
--         ]


viewErrors : List String -> Html Action
viewErrors errors =
    ul
        []
        ((List.map (\error -> li [] [ Html.text error ]) errors))


-- aliasCss : Css.Snippet
-- aliasCss =
--     (.) Alias
--         [ padding2 (px 20) zero
--         , children
--             [ Css.input
--                 [ padding (px 10)
--                 , marginLeft (px 10)
--                 , Css.width (px 250)
--                 ]
--             ]
--         ]


viewNameSelect : String -> Html Action
viewNameSelect name =
    div
        [
        -- class [ Alias ]
        ]
        [ label [] [ Html.text "Enter a toplevel alias name here: " ]
        , input
            [ onInput UpdateName
            , style "top" "0px"
            , value name
            ]
            [ Html.text <| name ]
        ]


view : Model -> Html Action
view model =
    let
        mainBody =
            case model.inputType of
                JsonBlob ->
                    [ viewAllAliases model.elmVersion model.input model.decoderType model.aliases
                    ]

                -- TypeAliasType ->
                --     [ viewTypeAliasStuff model.elmVersion model.input
                --     ]

                -- UnionType ->
                --     [ viewAllUnions model.input
                --     ]

                -- DecoderString ->
                --     [ viewAllDecoder model.elmVersion model.decoderType model.input
                --     ]
    in
        div
            [
            -- class [ Content ] 
            ]
            [ Util.stylesheetLink "./elm/homepage.css"
            , case model.inputType of
            JsonBlob ->
                viewNameSelect model.name
        --  , viewDecoderTypeInput model.decoderType
            , div
                [ style "height" "500px"
                , style "width" "100%"
                ]
                ([ viewInput model.input ]
                    ++ mainBody
                )
            ]


type alias Field =
    { name : String
    , typeName : String
    , base : String
    , value : Json.Value
    }


type alias Stuff =
    { stuff : Json.Value
    , fields : List Field
    }


type Action
    = UpdateInput String
    | UpdateName String
    -- | UpdateDecoder DecoderType
    | Noop
    | UpdateValue Stuff


type InputType
    = JsonBlob
    -- = TypeAliasType
    -- | UnionType
    -- | JsonBlob
    -- | DecoderString


type DecoderType
    = Pipeline
    -- = Original
    -- | Pipeline
    -- | English


type ElmVersion
    = O18
    -- = O17
    -- | O18


type alias Model =
    { input : String
    , name : String
    , errors : List String
    , inputType : InputType
    , decoderType : DecoderType
    , elmVersion : ElmVersion
    , aliases : List TypeAlias.TypeAlias
    }


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ Sub.none
        , updateValue UpdateValue
        ]


port toValue : Json.Value -> Cmd msg


port updateValue : (Stuff -> msg) -> Sub msg


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Noop ->
            ( model, Cmd.none )

        UpdateValue { stuff, fields } ->
            ( { model | aliases = List.map (\field ->
                    { base = field.base
                    , name = field.name
                    , typeName = field.typeName
                        |> Types.typeToKnownTypes
                    , value = field.value
                    }
                ) fields
                    |> TypeAlias.createTypeAliases stuff
            }
            , Cmd.none )

        UpdateInput input ->
            case String.trim input of
                "" ->
                    ( { model | input = "" }
                    , Cmd.none
                    )
            
                trimmed ->
                    ( { model
                        | input = trimmed
                        -- , inputType =
                        --     if TypeAlias.isUnionType trimmed then
                        --         UnionType
                        --     else if TypeAlias.isTypeAlias trimmed then
                        --         TypeAliasType
                        --     else if TypeAlias.isDecoder trimmed then
                        --         DecoderString
                        --     else
                        --         JsonBlob
                    }
                -- , mapPort <| E.object
                --     [ ("Cmd", E.string "Fly")
                --     , ("lon", lonLatValue "lon" model)
                --     , ("lat", lonLatValue "lat" model)
                --     , ("zoom", E.float model.zoom)
                --     , ("geoJson", geoJsonValue model)
                --     ]
                    , toValue <| Json.object
                        [ ( "aliasName", Json.string model.name )
                        , ( "text", Json.string trimmed )
                        ]
                    )

        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        -- UpdateDecoder decoder ->
        --     ( { model | decoderType = decoder }, Cmd.none )


defaultModel : Model
defaultModel =
    { input = """"""
    , name = "Something"
    , errors = []
    , inputType = JsonBlob
    , decoderType = Pipeline
    , elmVersion = O18
    , aliases = []
    }
