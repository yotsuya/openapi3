{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.OpenApi.Schema.ValidationSpec (spec) where

import           Control.Lens                  (at, (^?!), _Just)
import           Control.Monad                 (forM_)
import           Data.Aeson                    (Value, decode)
import           Data.ByteString.Lazy          (ByteString)
import           Data.Maybe                    (fromJust)
import           Data.String.Here              (here)
import           Test.Hspec

import           Data.OpenApi                  (Definitions, OpenApi, Referenced, RequestBody,
                                                Schema, components, content, paths, post,
                                                requestBodies, requestBody, schema, schemas,
                                                validateJSON)
import           Data.OpenApi.Schema.Generator (dereference)


makeParams :: ByteString -> (Definitions Schema, Schema)
makeParams schemaJson = (ds, s)
  where
    openApi = makeOpenApi schemaJson
    ds = getDefinitionsSchema openApi
    s = getSchema openApi

makeOpenApi :: ByteString -> OpenApi
makeOpenApi schemaJson = fromJust $ decode openApiJson
  where
    openApiJson :: ByteString
    openApiJson = [here|
{
    "openapi": "3.0.0",
    "info": { "title": "info example", "version": "1.0.0" },
    "paths": {
        "/examples": {
            "post": {
                "requestBody": {
                    "content": {
                        "application/json": {
                            "schema": {
                                "$ref": "#/components/schemas/Example"
                            }
                        }
                    }
                },
                "responses": { "default": { "description": "response example" } }
            }
        }
    },
    "components": {
        "schemas": {
            "Example": {
                "type": "object",
                "properties": |] <> schemaJson <> [here|
            }
        }
    }
}
|]

getDefinitionsSchema :: OpenApi -> Definitions Schema
getDefinitionsSchema openApi = openApi ^?! (components . schemas)

getSchema :: OpenApi -> Schema
getSchema openApi = dereference (getDefinitionsSchema openApi) referencedSchema
  where
    referencedSchema :: Referenced Schema
    referencedSchema = postRequestBody ^?! (content . at "application/json" . _Just . schema . _Just)

    postRequestBody :: RequestBody
    postRequestBody = dereference definitionsRequestBody referencedRequestBody

    definitionsRequestBody :: Definitions RequestBody
    definitionsRequestBody = openApi ^?! (components . requestBodies)

    referencedRequestBody :: Referenced RequestBody
    referencedRequestBody = openApi ^?! (paths . at "/examples" . _Just . post . _Just . requestBody . _Just)


toValue :: ByteString -> Value
toValue json = fromJust $ decode json

spec :: Spec
spec =
    describe "validateJSON" $ do

---------------------------------------------------------------------------------------------------
        context "type: integer" $ do

            it "returns [] if integerに整数" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer"}} |]
                    v = toValue [here| {"mint": 1} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if integerなのに小数部を含むnumber" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer"}} |]
                    v = toValue [here| {"mint": 1.1} |]
                validateJSON ds s v `shouldBe` ["not an integer"]

            it "returns エラー if integerなのに別の型の値" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer"}} |]
                    vs =
                        [ toValue [here| {"mint": "text"} |]
                        , toValue [here| {"mint": true} |]
                        , toValue [here| {"mint": [0, 1, 2]} |]
                        , toValue [here| {"mint": {"k": "v"}} |]
                        ]
                forM_ vs $ \v ->
                    -- TODO: エラーメッセージ改善
                    validateJSON ds s v `shouldBe` ["expected JSON value of type OpenApiInteger"]

            it "returns [] if minimumと同じ" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "minimum": 10}} |]
                    v = toValue [here| {"mint": 10} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if minimumより小さい" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "minimum": 10}} |]
                    v = toValue [here| {"mint": 9} |]
                validateJSON ds s v `shouldBe` ["value 9.0 falls below minimum (should be >=10.0)"]

            it "returns [] if minimumと同じ&&exclusiveMinimum:true" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "minimum": 10, "exclusiveMinimum": true}} |]
                    v = toValue [here| {"mint": 10} |]
                -- TODO: バグ報告 エラーメッセージの数字が欠落している
                validateJSON ds s v `shouldBe` ["value 10.0 falls below minimum (should be >"]

            it "returns [] if minimumと同じ&&exclusiveMinimum:false" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "minimum": 10, "exclusiveMinimum": false}} |]
                    v = toValue [here| {"mint": 10} |]
                validateJSON ds s v `shouldBe` []

            it "returns [] if maximumと同じ" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "maximum": 10}} |]
                    v = toValue [here| {"mint": 10} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if maximumより大きい" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "maximum": 10}} |]
                    v = toValue [here| {"mint": 11} |]
                validateJSON ds s v `shouldBe` ["value 11.0 exceeds maximum (should be <=10.0)"]

            it "returns [] if maximumと同じ&&exclusiveMaximum:true" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "maximum": 10, "exclusiveMaximum": true}} |]
                    v = toValue [here| {"mint": 11} |]
                -- TODO: バグ報告 エラーメッセージの数字が欠落している
                validateJSON ds s v `shouldBe` ["value 11.0 exceeds maximum (should be <"]

            it "returns [] if maximumと同じ&&exclusiveMaximum:false" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "maximum": 10, "exclusiveMaximum": false}} |]
                    v = toValue [here| {"mint": 10} |]
                validateJSON ds s v `shouldBe` []

            it "returns [] if multipleOfの倍数の値" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "multipleOf": 5}} |]
                    v = toValue [here| {"mint": 10} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if multipleOfの倍数じゃない値" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "multipleOf": 4}} |]
                    v = toValue [here| {"mint": 10} |]
                validateJSON ds s v `shouldBe` ["expected a multiple of 4.0 but got 10.0"]

            it "returns [] if int32の最大値" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "format": "int32"}} |]
                    v = toValue [here| {"mint": 2147483647} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if int32の最大値より大きい値" $ do
                -- TODO: バグ報告
                pendingWith "format:int32に未対応"
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "format": "int32"}} |]
                    v = toValue [here| {"mint": 2147483648} |]
                validateJSON ds s v `shouldBe` []

---------------------------------------------------------------------------------------------------
        context "type: number" $ do

            it "returns [] if numberに小数部を含む数" $ do
                let
                    (ds, s) = makeParams [here| {"mnum": {"type": "number"}} |]
                    v = toValue [here| {"mnum": 1.1} |]
                validateJSON ds s v `shouldBe` []

            it "returns [] if numberなのに整数" $ do
                let
                    (ds, s) = makeParams [here| {"mnum": {"type": "number"}} |]
                    v = toValue [here| {"mnum": 1} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if numberなのに別の型の値" $ do
                let
                    (ds, s) = makeParams [here| {"mnum": {"type": "number"}} |]
                    vs =
                        [ toValue [here| {"mnum": "text"} |]
                        , toValue [here| {"mnum": true} |]
                        , toValue [here| {"mnum": [0, 1, 2 ]} |]
                        , toValue [here| {"mnum": {"k": "v"}} |]
                        ]
                forM_ vs $ \v ->
                    -- TODO: エラーメッセージ改善
                    validateJSON ds s v `shouldBe` ["expected JSON value of type OpenApiNumber"]

---------------------------------------------------------------------------------------------------
        context "type: string" $ do

            it "returns [] if stringに文字列" $ do
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string"}} |]
                    v = toValue [here| {"mstr": "text"} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if stringなのに別の型の値" $ do
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string"}} |]
                    vs =
                        [ toValue [here| {"mstr": 1} |]
                        , toValue [here| {"mstr": 1.1} |]
                        , toValue [here| {"mstr": true} |]
                        , toValue [here| {"mstr": [0, 1, 2]} |]
                        , toValue [here| {"mstr": {"k": "v"}} |]
                        ]
                forM_ vs $ \v ->
                    -- TODO: エラーメッセージ改善
                    validateJSON ds s v `shouldBe` ["expected JSON value of type OpenApiString"]

            it "returns [] if minLengthと同じ" $ do
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string", "minLength": 3}} |]
                    v = toValue [here| { "mstr": "abc" } |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if minLengthより小さい" $ do
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string", "minLength": 4}} |]
                    v = toValue [here| { "mstr": "abc" } |]
                validateJSON ds s v `shouldBe` ["string is too short (length should be >=4)"]

            it "returns [] if maxLengthと同じ" $ do
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string", "maxLength": 3}} |]
                    v = toValue [here| { "mstr": "abc" } |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if maxLengthより小さい" $ do
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string", "maxLength": 2}} |]
                    v = toValue [here| { "mstr": "abc" } |]
                validateJSON ds s v `shouldBe` ["string is too long (length should be <=2)"]

            it "returns [] if format:dateの正しい文字列" $ do
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string", "format": "date"}} |]
                    v = toValue [here| { "mstr": "2017-07-21" } |]
                validateJSON ds s v `shouldBe` []

            it "returns [] if format:date違反の文字列" $ do
                -- TODO: バグ報告
                pendingWith "format:dateに未対応"
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string", "format": "date"}} |]
                    v = toValue [here| { "mstr": "not date!" } |]
                validateJSON ds s v `shouldNotBe` []

            it "returns [] if format:date-timeの正しい文字列" $ do
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string", "format": "date-time"}} |]
                    v = toValue [here| { "mstr": "2017-07-21T17:32:28Z" } |]
                validateJSON ds s v `shouldBe` []

            it "returns [] if format:date-time違反の文字列" $ do
                -- TODO: バグ報告
                pendingWith "format:date-timeに未対応"
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string", "format": "date-time"}} |]
                    v = toValue [here| {"mstr": "not datetime!"} |]
                validateJSON ds s v `shouldNotBe` []

            it "returns [] if format:byteの正しい文字列" $ do
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string", "format": "byte"}} |]
                    v = toValue [here| {"mstr": "U3dhZ2dlciByb2Nrcw=="} |]
                validateJSON ds s v `shouldBe` []

            it "returns [] if format:byte違反の文字列" $ do
                -- TODO: バグ報告
                pendingWith "format:byteに未対応"
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string", "format": "byte"}} |]
                    v = toValue [here| {"mstr": "not byte!"} |]
                validateJSON ds s v `shouldNotBe` []

            it "returns [] if patternにマッチする文字列" $ do
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string", "pattern": "^abc$"}} |]
                    v = toValue [here| {"mstr": "abc"} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if patternにマッチしない文字列" $ do
                -- TODO: バグ報告
                pendingWith "patternに未対応"
                let
                    (ds, s) = makeParams [here| {"mstr": {"type": "string", "pattern": "^abc$"}} |]
                    v = toValue [here| {"mstr": "def"} |]
                validateJSON ds s v `shouldNotBe` []

---------------------------------------------------------------------------------------------------
        context "type: boolean" $ do

            it "returns [] if booleanに真偽値" $ do
                let
                    (ds, s) = makeParams [here| {"mbol": {"type": "boolean"}} |]
                    v = toValue [here| {"mbol": true} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if booleanなのに別の型の値" $ do
                let
                    (ds, s) = makeParams [here| {"mbol": {"type": "boolean"}} |]
                    vs =
                        [ toValue [here| {"mbol": 1} |]
                        , toValue [here| {"mbol": 1.1} |]
                        , toValue [here| {"mbol": "text"} |]
                        , toValue [here| {"mbol": [0, 1, 2]} |]
                        , toValue [here| {"mbol": {"k": "v"}} |]
                        ]
                forM_ vs $ \v ->
                    -- TODO: エラーメッセージ改善
                    validateJSON ds s v `shouldBe` ["expected JSON value of type OpenApiBoolean"]

---------------------------------------------------------------------------------------------------
        context "type: array" $ do

            it "returns [] if 正しいデータ型の値 for リスト" $ do
                let
                    (ds, s) = makeParams [here| {"mlst": {"type": "array", "items": {"type": "integer"}}} |]
                    v = toValue [here| {"mlst": [1, 2]} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if 異なるデータ型の値 for リスト" $ do
                let
                    (ds, s) = makeParams [here| {"mlst": {"type": "array", "items": {"type": "integer"}}} |]
                    v = toValue [here| {"mlst": [1, "2"]} |]
                validateJSON ds s v `shouldNotBe` []

            it "returns [] if 正しいデータ型の値 for タプル" $ do
                let
                    (ds, s) = makeParams [here| {"mtpl": {"type": "array", "items": [{"type": "integer"}, {"type": "string"}]}} |]
                    v = toValue [here| {"mtpl": [1, "text"]} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if 異なるデータ型の値 for タプル" $ do
                let
                    (ds, s) = makeParams [here| {"mtpl": {"type": "array", "items": [{"type": "integer"}, {"type": "string"}]}} |]
                    v = toValue [here| {"mtpl": [1, 2]} |]
                validateJSON ds s v `shouldNotBe` []

            it "returns エラー if minItemsより少ない要素数" $ do
                let
                    (ds, s) = makeParams [here| {"mlst": {"type": "array", "items": {"type": "integer"}, "minItems": 2}} |]
                    v = toValue [here| {"mlst": [1]} |]
                validateJSON ds s v `shouldBe` ["array is too short (size should be >=2)"]

            it "returns エラー if maxItemsより多い要素数" $ do
                let
                    (ds, s) = makeParams [here| {"mlst": {"type": "array", "items": {"type": "integer"}, "maxItems": 2}} |]
                    v = toValue [here| {"mlst": [1, 2, 3]} |]
                validateJSON ds s v `shouldBe` ["array exceeds maximum size (should be <=2)"]

            it "returns [] if uniqueItems指定無しのときに要素の重複" $ do
                let
                    (ds, s) = makeParams [here| {"mlst": {"type": "array", "items": {"type": "integer"}}} |]
                    v = toValue [here| {"mlst": [1, 2, 1]} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if uniqueItems:trueのときに要素の重複" $ do
                let
                    (ds, s) = makeParams [here| {"mlst": {"type": "array", "items": {"type": "integer"}, "uniqueItems": true}} |]
                    v = toValue [here| {"mlst": [1, 2, 1]} |]
                validateJSON ds s v `shouldBe` ["array is expected to contain unique items, but it does not"]

            it "returns エラー if uniqueItems:falseのときに要素の重複" $ do
                let
                    (ds, s) = makeParams [here| {"mlst": {"type": "array", "items": {"type": "integer"}, "uniqueItems": false}} |]
                    v = toValue [here| {"mlst": [1, 2, 1]} |]
                validateJSON ds s v `shouldBe` []

        context "type:object" $ do

            it "returns エラー if 未定義なプロパティがある" $ do
                let
                    (ds, s) = makeParams [here|
{
    "mobj": {
        "type": "object",
        "properties": {
            "mint": {"type": "integer"}
        }
    }
}
|]
                    v = toValue [here| {"mobj": {"mstr": "hello"}} |]
                validateJSON ds s v `shouldBe` ["property \"mstr\" is found in JSON value, but it is not mentioned in Swagger schema"]

            it "returns [] if requiredなプロパティがある" $ do
                let
                    (ds, s) = makeParams [here|
{
    "mobj": {
        "type": "object",
        "required": ["cint", "cstr"],
        "properties": {
            "cint": {"type": "integer"},
            "cstr": {"type": "string"},
            "mbol": {"type": "boolean"}
        }
    }
}
|]
                    v = toValue [here| {"mobj": {"cint": 1, "cstr": "hello"}} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if requiredなプロパティがない" $ do
                let
                    (ds, s) = makeParams [here|
{
    "mobj": {
        "type": "object",
        "required": ["cint", "cstr"],
        "properties": {
            "cint": {"type": "integer"},
            "cstr": {"type": "string"},
            "mbol": {"type": "boolean"}
        }
    }
}
|]
                    v = toValue [here| {"mobj": {"cint": 1, "mbol": true}} |]
                validateJSON ds s v `shouldBe` ["property \"cstr\" is required, but not found in \"{\\\"mbol\\\":true,\\\"cint\\\":1}\""]

            it "returns [] if プロパティ数がminPropertiesより多い" $ do
                let
                    (ds, s) = makeParams [here|
{
    "mobj": {
        "type": "object",
        "minProperties": 2,
        "properties": {
            "mint": {"type": "integer"},
            "mstr": {"type": "string"},
            "mbol": {"type": "boolean"}
        }
    }
}
|]
                    v = toValue [here| {"mobj": {"mint": 1, "mstr": "text"}} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if プロパティ数がminPropertiesより少ない" $ do
                let
                    (ds, s) = makeParams [here|
{
    "mobj": {
        "type": "object",
        "minProperties": 2,
        "properties": {
            "mint": {"type": "integer"},
            "mstr": {"type": "string"},
            "mbol": {"type": "boolean"}
        }
    }
}
|]
                    v = toValue [here| {"mobj": {"mint": 1}} |]
                validateJSON ds s v `shouldBe` ["object size is too small (total number of properties should be >=2)"]

            it "returns [] if プロパティ数がmaxProperties以下" $ do
                let
                    (ds, s) = makeParams [here|
{
    "mobj": {
        "type": "object",
        "maxProperties": 2,
        "properties": {
            "mint": {"type": "integer"},
            "mstr": {"type": "string"},
            "mbol": {"type": "boolean"}
        }
    }
}
|]
                    v = toValue [here| {"mobj": {"mint": 1, "mstr": "text"}} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if プロパティ数がmaxPropertiesより多い" $ do
                let
                    (ds, s) = makeParams [here|
{
    "mobj": {
        "type": "object",
        "maxProperties": 2,
        "properties": {
            "mint": {"type": "integer"},
            "mstr": {"type": "string"},
            "mbol": {"type": "boolean"}
        }
    }
}
|]
                    v = toValue [here| {"mobj": {"mint": 1, "mstr": "text", "mbol": true}} |]
                validateJSON ds s v `shouldBe` ["object size exceeds maximum (total number of properties should be <=2)"]

---------------------------------------------------------------------------------------------------
        context "enum" $ do

            it "returns [] if enumに指定されている値" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "enum": [10, 20, 30]}} |]
                    v = toValue [here| {"mint": 10} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if enumに指定されていない値" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "enum": [20, 30]}} |]
                    v = toValue [here| {"mint": 10} |]
                validateJSON ds s v `shouldBe` ["expected one of \"[20,30]\" but got Number 10.0"]

---------------------------------------------------------------------------------------------------
        context "nullable" $ do

            it "returns [] if nullableにnull" $ do
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "nullable": true}} |]
                    v = toValue [here| {"mint": null} |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if not nullableにnull" $ do
                -- TODO: バグ報告
                pendingWith "nullableに未対応"
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer", "nullable": false}} |]
                    v = toValue [here| {"mint": null} |]
                validateJSON ds s v `shouldNotBe` []

            it "returns エラー if nullable指定無し（デフォルト＝not nullable）にnull" $ do
                -- TODO: バグ報告
                pendingWith "nullableに未対応"
                let
                    (ds, s) = makeParams [here| {"mint": {"type": "integer"}} |]
                    v = toValue [here| {"mint": null} |]
                validateJSON ds s v `shouldNotBe` []
