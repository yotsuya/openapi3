{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.OpenApi.InternalSpec (spec) where

import           Data.Aeson            (decode)
import           Data.ByteString.Lazy  (ByteString)
import           Data.Maybe            (isJust)
import           Data.String.Here      (here)
import           Test.Hspec

import           Data.OpenApi.Internal (OpenApi)


decode' :: ByteString -> Maybe OpenApi
decode' = decode

openApiJson :: ByteString
openApiJson = [here|
{
    "openapi": "3.0.0",
    "info": { "title": "info example", "version": "1.0.0" },
    "paths": {
        "/pets": {
            "post": {
                "requestBody": {
                    "content": {
                        "application/json": {
                            "schema": {
                                "$ref": "#/components/schemas/Pet"
                            }
                        }
                    }
                },
                "responses": { "default": { "description": "response example" } }
            }
        },
        "/pets/{petId}": {
            "parameters": [
                {
                    "name": "petId",
                    "in": "path",
                    "required": true,
                    "schema": { "type": "integer" }
                }
            ],
            "put": {
                "requestBody": {
                    "content": {
                        "application/json": {
                            "schema": {
                                "$ref": "#/components/schemas/Pet"
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
            "Pet": {
                "type": "object",
                "required": [ "name" ],
                "properties": {
                    "name": { "type": "string" },
                    "age": { "type": "integer" }
                }
            }
        }
    }
}
|]

spec :: Spec
spec =
    describe "decode" $ do
        it "returns Just OpenApi if the OpenAPI document is valid" $
            decode' openApiJson `shouldSatisfy` isJust

        it "returns Nothing if the OpenAPI document is invalid" $
            decode' "" `shouldBe` Nothing

        -- TODO: report to openapi3 team
        it "returns Just OpenApi even if the OpenAPI document has no components" $ do
            pending
            let json = [here|
{
    "openapi": "3.0.0",
    "info": { "title": "info example", "version": "1.0.0" },
    "paths": {
        "/pets": {
            "post": {
                "requestBody": {
                    "content": {
                        "application/json": {
                            "schema": {
                                "type": "object",
                                "required": [ "name" ],
                                "properties": {
                                    "name": { "type": "string" },
                                    "age": { "type": "integer" }
                                }
                            }
                        }
                    }
                },
                "responses": { "default": { "description": "response example" } }
            }
        }
    }
}
|]
            decode' json `shouldSatisfy` isJust

        -- TODO: report to openapi3 team
        it "returns Nothing if the OpenAPI document has no Path objects" $ do
            pending
            let json = [here|
{
    "openapi": "3.0.0",
    "info": { "title": "info example", "version": "1.0.0" },
    "components": {
    }
}
|]
            decode' json `shouldBe` Nothing
