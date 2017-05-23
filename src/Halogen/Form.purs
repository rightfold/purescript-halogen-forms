module Halogen.Form
  ( Form(..)

  , string
  , string'
  ) where

import Data.Validation.Semigroup (V)
import Halogen.HTML (HTML)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Prelude

--------------------------------------------------------------------------------

-- | A form with slots `p`, validation errors `e`, state `s`, and value `a`.
newtype Form p e s a = Form
  { initialState :: s
  , fromStructure :: a -> s
  , toStructure :: s -> V e a
  , render :: s -> Array (HTML p s)
  }

--------------------------------------------------------------------------------

-- | A form that edits any string.
string :: ∀ p e. Semigroup e => Form p e String String
string = string' ""

-- | A form that edits any string, with a default value.
string' :: ∀ p e. Semigroup e => String -> Form p e String String
string' initialState = Form
  { initialState
  , fromStructure: id
  , toStructure: pure
  , render: \s ->
      [ H.input [ P.value s
                , E.onValueChange pure
                ]
      ]
  }
