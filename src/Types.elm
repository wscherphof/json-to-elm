module Types exposing (..)

import Json.Encode as Json
import String


type KnownTypes
    = MaybeType KnownTypes
    | ListType KnownTypes
    | IntType
    | FloatType
    | BoolType
    | StringType
    | ComplexType
    | ResolvedType String
    | Unknown


typeToKnownTypes : String -> KnownTypes
typeToKnownTypes string =
    case string of
        "Int" ->
            IntType

        "Float" ->
            FloatType

        "Bool" ->
            BoolType

        "String" ->
            StringType

        "Something" ->
            ComplexType

        _ ->
            case String.words string of
                [] ->
                    Unknown

                [ x ] ->
                    typeToKnownTypes "Something"

                x :: xs ->
                    case x of
                        "Maybe" ->
                            MaybeType (typeToKnownTypes <| String.join " " xs)

                        "List" ->
                            ListType (typeToKnownTypes <| String.join " " xs)

                        _ ->
                            Unknown


knownTypesToString : KnownTypes -> String
knownTypesToString known =
    case known of
        Unknown ->
            "_Unknown"

        ComplexType ->
            "ComplexType"

        ResolvedType name ->
            String.trim name

        IntType ->
            "Int"

        FloatType ->
            "Float"

        StringType ->
            "String"

        BoolType ->
            "Bool"

        ListType nested ->
            "List " ++ (knownTypesToString nested)

        MaybeType nested ->
            "Maybe " ++ (knownTypesToString nested)


knownTypesToEnglish : KnownTypes -> String
knownTypesToEnglish known =
    case known of
        Unknown ->
            "I don't know what this is"

        ComplexType ->
            "something that has to be written by hand!"

        ResolvedType name ->
            "a type that you've called " ++ (String.trim name)

        IntType ->
            "an int value"

        FloatType ->
            "a float value"

        StringType ->
            "a string value"

        BoolType ->
            "a boolean value"

        ListType nested ->
            "a list of " ++ (knownTypesToString nested)

        MaybeType nested ->
            "an optional value of " ++ (knownTypesToString nested)
