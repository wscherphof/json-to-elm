port module Home exposing (..)

import TypeAlias exposing (TypeAlias)
import TypeAlias.O19
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
            TypeAlias.O19.createPipelineDecoder

        imports =
            TypeAlias.O19.pipelineImports

        encoder =
            TypeAlias.O19.createEncoder

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
            TypeAlias.O19.createDecoder incoming

        encoder =
            TypeAlias.O19.createEncoder incoming

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


viewInput : String -> Html Action
viewInput alias =
    Html.textarea
        [ onInput UpdateInput
        -- , class [ Input ]
        , style "height" "100%"
        , style "width" "40%"
        , spellcheck False
        , placeholder "Put a valid JSON object in here!"
        ]
        [ Html.text <| alias ]


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
    in
        div
            [
            -- class [ Content ] 
            ]
            [ Util.stylesheetLink "./elm/homepage.css"
            , case model.inputType of
            JsonBlob ->
                viewNameSelect model.name
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


type Action
    = UpdateInput String
    | UpdateName String
    | Noop
    | UpdateFields (List Field)


type InputType
    = JsonBlob


type DecoderType
    = Pipeline


type ElmVersion
    = O19


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
        , updateFields UpdateFields
        ]


port parse : Json.Value -> Cmd msg


port updateFields : (List Field -> msg) -> Sub msg


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Noop ->
            ( model
            , Cmd.none
            )

        UpdateFields fields ->
            ( { model | aliases =
                TypeAlias.createTypeAliases
                    <| List.map (\field ->
                        { base = field.base
                        , name = field.name
                        , typeName = field.typeName
                            |> Types.typeToKnownTypes
                        , value = field.value
                        }
                    ) fields
            }
            , Cmd.none
            )

        UpdateInput input ->
            case String.trim input of
                "" ->
                    ( { model | input = "" }
                    , Cmd.none
                    )
            
                trimmed ->
                    ( { model | input = trimmed }
                    , parse <| Json.object
                        [ ( "aliasName", Json.string model.name )
                        , ( "text", Json.string trimmed )
                        ]
                    )

        UpdateName name ->
            ( { model | name = name }
            , Cmd.none
            )


defaultModel : Model
defaultModel =
    { input = """"""
    , name = "Something"
    , errors = []
    , inputType = JsonBlob
    , decoderType = Pipeline
    , elmVersion = O19
    , aliases = []
    }
