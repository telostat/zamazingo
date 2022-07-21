{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TestAeson where

import qualified Data.Aeson      as Aeson
import           GHC.Generics    (Generic)
import           GHC.Stack       (HasCallStack)
import           Zamazingo.Aeson (commonAesonOptions)


run :: IO ()
run = do
  let student = Student 1 "A"
  let studentJson = "{\"id\":1,\"name\":\"A\"}"
  assert (Aeson.encode student == studentJson) $ pure ()
  assert (Aeson.eitherDecode studentJson == Right student) $ pure ()

  let op = OpAdd
  let opJson = "\"add\""
  assert (Aeson.encode op == opJson) $ pure ()
  assert (Aeson.eitherDecode opJson == Right op) $ pure ()

  let state1 = StateLoading
  let state1Json = "{\"type\":\"loading\"}"
  assert (Aeson.encode state1 == state1Json) $ pure ()
  assert (Aeson.eitherDecode state1Json == Right state1) $ pure ()

  let state2 = StateError "unknown"
  let state2Json = "{\"type\":\"error\",\"value\":\"unknown\"}"
  assert (Aeson.encode state2 == state2Json) $ pure ()
  assert (Aeson.eitherDecode state2Json == Right state2) $ pure ()

  let state3 = StateResult 1
  let state3Json = "{\"type\":\"result\",\"value\":1}"
  assert (Aeson.encode state3 == state3Json) $ pure ()
  assert (Aeson.eitherDecode state3Json == Right state3) $ pure ()


data Student = Student
  { studentId   :: !Int
  , studentName :: !String
  }
  deriving (Eq, Generic, Show)


instance Aeson.FromJSON Student where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "student"


instance Aeson.ToJSON Student where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "student"


data Op = OpAdd | OpSub | OpMul | OpDiv
  deriving (Eq, Generic, Show)


instance Aeson.FromJSON Op where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "Op"


instance Aeson.ToJSON Op where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "Op"


data State =
    StateLoading
  | StateError String
  | StateResult Int
  deriving (Eq, Generic, Show)


instance Aeson.FromJSON State where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "State"


instance Aeson.ToJSON State where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "State"


assert :: HasCallStack => Bool -> a -> a
assert False _ = error "Assertion error"
assert True a  = a
