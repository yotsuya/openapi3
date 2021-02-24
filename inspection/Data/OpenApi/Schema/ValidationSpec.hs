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
            "Example": |] <> schemaJson <> [here|
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
        context "スカラー型のチェック" $ do
            let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": { "type": "integer" },
        "mnum": { "type": "number" },
        "mstr": { "type": "string" },
        "mbol": { "type": "boolean" }
    }
}
|]

            it "returns [] if 正しいデータ型の値" $ do
                let v = toValue [here|
{
    "mint": 1,
    "mnum": 1.1,
    "mstr": "text",
    "mbol": true
}
|]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if integerなのに小数部を含むnumber" $ do
                let v = toValue [here| { "mint": 1.1 } |]
                validateJSON ds s v `shouldBe` ["not an integer"]

            it "returns エラー if integerなのに別の型の値" $ do
                let vs =
                        [ toValue [here| { "mint": "text" } |]
                        , toValue [here| { "mint": true } |]
                        , toValue [here| { "mint": [ 0, 1, 2 ] } |]
                        , toValue [here| { "mint": { "k": "v" } } |]
                        ]
                forM_ vs $ \v ->
                    -- TODO: エラーメッセージ改善
                    validateJSON ds s v `shouldBe` ["expected JSON value of type OpenApiInteger"]

            it "returns [] if numberなのに整数" $ do
                let v = toValue [here| { "mnum": 1 } |]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if numberなのに別の型の値" $ do
                let vs =
                        [ toValue [here| { "mnum": "text" } |]
                        , toValue [here| { "mnum": true } |]
                        , toValue [here| { "mnum": [ 0, 1, 2 ] } |]
                        , toValue [here| { "mnum": { "k": "v" } } |]
                        ]
                forM_ vs $ \v ->
                    -- TODO: エラーメッセージ改善
                    validateJSON ds s v `shouldBe` ["expected JSON value of type OpenApiNumber"]

            it "returns エラー if stringなのに別の型の値" $ do
                let vs =
                        [ toValue [here| { "mstr": 1 } |]
                        , toValue [here| { "mstr": 1.1 } |]
                        , toValue [here| { "mstr": true } |]
                        , toValue [here| { "mstr": [ 0, 1, 2 ] } |]
                        , toValue [here| { "mstr": { "k": "v" } } |]
                        ]
                forM_ vs $ \v ->
                    -- TODO: エラーメッセージ改善
                    validateJSON ds s v `shouldBe` ["expected JSON value of type OpenApiString"]

            it "returns エラー if booleanなのに別の型の値" $ do
                let vs =
                        [ toValue [here| { "mbol": 1 } |]
                        , toValue [here| { "mbol": 1.1 } |]
                        , toValue [here| { "mbol": "text" } |]
                        , toValue [here| { "mbol": [ 0, 1, 2 ] } |]
                        , toValue [here| { "mbol": { "k": "v" } } |]
                        ]
                forM_ vs $ \v ->
                    -- TODO: エラーメッセージ改善
                    validateJSON ds s v `shouldBe` ["expected JSON value of type OpenApiBoolean"]

---------------------------------------------------------------------------------------------------
        context "integer型のチェック" $ do
            let v = toValue [here| { "mint": 10 } |]

            it "returns [] if minimumと同じ" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": {
            "type": "integer",
            "minimum": 10
        }
    }
}
|]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if minimumより小さい" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": {
            "type": "integer",
            "minimum": 11
        }
    }
}
|]
                validateJSON ds s v `shouldBe` ["value 10.0 falls below minimum (should be >=11.0)"]

            it "returns [] if minimumと同じ&&exclusiveMinimum:true" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": {
            "type": "integer",
            "minimum": 10,
            "exclusiveMinimum": true
        }
    }
}
|]
                -- TODO: バグ報告 エラーメッセージの数字が欠落している
                validateJSON ds s v `shouldBe` ["value 10.0 falls below minimum (should be >"]

            it "returns [] if minimumと同じ&&exclusiveMinimum:false" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": {
            "type": "integer",
            "minimum": 10,
            "exclusiveMinimum": false
        }
    }
}
|]
                validateJSON ds s v `shouldBe` []

            it "returns [] if maximumと同じ" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": {
            "type": "integer",
            "maximum": 10
        }
    }
}
|]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if maximumより大きい" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": {
            "type": "integer",
            "maximum": 9
        }
    }
}
|]
                validateJSON ds s v `shouldBe` ["value 10.0 exceeds maximum (should be <=9.0)"]

            it "returns [] if maximumと同じ&&exclusiveMaximum:true" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": {
            "type": "integer",
            "maximum": 10,
            "exclusiveMaximum": true
        }
    }
}
|]
                -- TODO: バグ報告 エラーメッセージの数字が欠落している
                validateJSON ds s v `shouldBe` ["value 10.0 exceeds maximum (should be <"]

            it "returns [] if maximumと同じ&&exclusiveMaximum:false" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": {
            "type": "integer",
            "maximum": 10,
            "exclusiveMaximum": false
        }
    }
}
|]
                validateJSON ds s v `shouldBe` []

            it "returns [] if enumに指定されている値" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": {
            "type": "integer",
            "enum": [10, 20, 30]
        }
    }
}
|]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if enumに指定されていない値" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": {
            "type": "integer",
            "enum": [20, 30]
        }
    }
}
|]
                validateJSON ds s v `shouldBe` ["expected one of \"[20,30]\" but got Number 10.0"]

            it "returns [] if multipleOfの倍数の値" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": {
            "type": "integer",
            "multipleOf": 5
        }
    }
}
|]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if multipleOfの倍数じゃない値" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": {
            "type": "integer",
            "multipleOf": 4
        }
    }
}
|]
                validateJSON ds s v `shouldBe` ["expected a multiple of 4.0 but got 10.0"]

---------------------------------------------------------------------------------------------------
        context "string型のチェック" $ do
            let v = toValue [here| { "mstr": "abc" } |]

            it "returns [] if minLengthと同じ" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mstr": {
            "type": "string",
            "minLength": 3
        }
    }
}
|]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if minLengthより小さい" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mstr": {
            "type": "string",
            "minLength": 4
        }
    }
}
|]
                validateJSON ds s v `shouldBe` ["string is too short (length should be >=4)"]

            it "returns [] if maxLengthと同じ" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mstr": {
            "type": "string",
            "maxLength": 3
        }
    }
}
|]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if maxLengthより小さい" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mstr": {
            "type": "string",
            "maxLength": 2
        }
    }
}
|]
                validateJSON ds s v `shouldBe` ["string is too long (length should be <=2)"]

---------------------------------------------------------------------------------------------------
        context "array型のitemsのチェック" $ do
            let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mlst": {
            "type": "array",
            "items": { "type": "integer" }
        },
        "mtpl": {
            "type": "array",
            "items": [
                { "type": "integer" },
                { "type": "string" }
            ]
        }
    }
}
|]

            it "returns [] if 正しいデータ型の値" $ do
                let v = toValue [here|
{
    "mlst": [1, 2],
    "mtpl": [1, "text"]
}
|]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if 異なるデータ型の値" $ do
                let v = toValue [here|
{
    "mlst": [1, "2"]
}
|]
                validateJSON ds s v `shouldNotBe` []

---------------------------------------------------------------------------------------------------
        context "array型のmin/maxItemsのチェック" $ do
            let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mlst": {
            "type": "array",
            "items": { "type": "integer" },
            "minItems": 2,
            "maxItems": 3
        }
    }
}
|]

            it "returns エラー if 少ない要素数" $ do
                let v = toValue [here|
{
    "mlst": [1]
}
|]
                validateJSON ds s v `shouldBe` ["array is too short (size should be >=2)"]

            it "returns エラー if 多い要素数" $ do
                let v = toValue [here|
{
    "mlst": [1, 2, 3, 4]
}
|]
                validateJSON ds s v `shouldBe` ["array exceeds maximum size (should be <=3)"]

        context "array型のuniqueItemsのチェック" $ do
            let v = toValue [here|
{
    "mlst": [1, 2, 1]
}
|]



            it "returns [] if uniqueItems指定無し" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mlst": {
            "type": "array",
            "items": { "type": "integer" }
        }
    }
}
|]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if uniqueItems:true" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mlst": {
            "type": "array",
            "items": { "type": "integer" },
            "uniqueItems": true
        }
    }
}
|]
                validateJSON ds s v `shouldBe` ["array is expected to contain unique items, but it does not"]

            it "returns エラー if uniqueItems:false" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mlst": {
            "type": "array",
            "items": { "type": "integer" },
            "uniqueItems": false
        }
    }
}
|]
                validateJSON ds s v `shouldBe` []

---------------------------------------------------------------------------------------------------
        context "nullableへの対応" $ do
            let v = toValue [here| { "mint": null } |]

            it "returns [] if nullableにnull" $ do
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": { "type": "integer", "nullable": true }
    }
}
|]
                validateJSON ds s v `shouldBe` []

            it "returns エラー if not nullableにnull" $ do
                -- TODO: バグ報告
                pendingWith "nullableに未対応"
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": { "type": "integer", "nullable": false }
    }
}
|]
                validateJSON ds s v `shouldNotBe` []

            it "returns エラー if nullable指定無し（デフォルト＝not nullable）にnull" $ do
                -- TODO: バグ報告
                pendingWith "nullableに未対応"
                let (ds, s) = makeParams [here|
{
    "type": "object",
    "properties": {
        "mint": { "type": "integer" }
    }
}
|]
                validateJSON ds s v `shouldNotBe` []
