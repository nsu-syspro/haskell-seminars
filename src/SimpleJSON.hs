{-# LANGUAGE OverlappingInstances #-}

module SimpleJSON where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)

-- value
--   object
--   array
--   string
--   number
--   "true"
--   "false"
--   "null"

data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Read)

render :: JValue -> String
render JNull        = "null"
render (JBool b)    = map toLower $ show b
render (JNumber d)  = show d
render (JString s)  = "\"" ++ s ++ "\""
render (JArray xs)  = "[" ++ intercalate ", " (map render xs) ++ "]"
render (JObject xs) = "{" ++ intercalate ", " (map renderPair xs) ++ "}"
 where
  renderPair :: (String, JValue) -> String
  renderPair (k, v) = "\"" ++ k ++ "\": " ++ render v

class JSON a where
  toJSON :: a -> JValue
  fromJSON :: JValue -> Maybe a

instance JSON JValue where
  toJSON = id
  fromJSON = Just

instance JSON Bool where
  toJSON = JBool
  fromJSON (JBool x) = Just x
  fromJSON _ = Nothing

instance JSON Double where
  toJSON = JNumber
  fromJSON (JNumber x) = Just x
  fromJSON _ = Nothing

instance JSON String where
  toJSON = JString
  fromJSON (JString x) = Just x
  fromJSON _ = Nothing

instance JSON a => JSON [a] where
  toJSON = JArray . map toJSON
  fromJSON (JArray x) = seqMap fromJSON x
  fromJSON _ = Nothing

instance JSON a => JSON [(String, a)] where
  toJSON = JObject . map (\(k, v) -> (k, toJSON v))
  fromJSON (JObject x) = seqMap fromJSONPair x
  fromJSON _ = Nothing

fromJSONPair :: JSON a => (String, JValue) -> Maybe (String, a)
fromJSONPair (k, v) = case fromJSON v of
  Just x -> Just (k, x)
  Nothing -> Nothing

seqMap :: (a -> Maybe b) -> [a] -> Maybe [b]
seqMap f [] = Just []
seqMap f (x : xs) = case f x of
  Nothing -> Nothing
  Just v  -> case seqMap f xs of
    Nothing -> Nothing
    Just vs -> Just $ v : vs

example :: JValue
example = JObject [
    ("first_name", JString "John"),
    ("last_name", JString "Smith"),
    ("is_alive", JBool True),
    ("age", JNumber 27),
    ("address", JObject [
      ("street_address", JString "21 2nd Street"),
      ("city", JString "New York"),
      ("state", JString "NY"),
      ("postal_code", JString "10021-3100")
    ]),
    ("phone_numbers", JArray [
      JObject [
        ("type", JString "home"),
        ("number", JString "212 555-1234")
      ],
      JObject [
        ("type", JString "office"),
        ("number", JString "646 555-4567")
      ]
    ]),
    ("children", JArray [
      JString "Catherine",
      JString "Thomas",
      JString "Trevor"
    ]),
    ("spouse", JNull)
  ]

data PhoneNumber = PhoneNumber { phoneType :: String, phoneNumber :: String }
 deriving (Show)

instance JSON PhoneNumber where
  toJSON (PhoneNumber t n) = toJSON [("type", t), ("number", n)]
  fromJSON = undefined

example2 :: JValue
example2 = toJSON [
    ("first_name", toJSON "John"),
    ("last_name", toJSON "Smith"),
    ("is_alive", toJSON True),
    ("age", toJSON (27 :: Double)),
    ("address", toJSON [
      ("street_address", "21 2nd Street"),
      ("city", "New York"),
      ("state", "NY"),
      ("postal_code", "10021-3100")
    ]),
    ("phone_numbers", toJSON [
      PhoneNumber "home" "212 555-1234",
      PhoneNumber "office" "646 55-4567"
    ]),
    ("children", toJSON [
      "Catherine",
      "Thomas",
      "Trevor"
    ]),
    ("spouse", JNull)
  ]
