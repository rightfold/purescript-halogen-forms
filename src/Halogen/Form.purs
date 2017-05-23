module Halogen.Form
  ( Form(..)

  , product

  , string
  , string'
  ) where

import Control.Apply (lift2)
import Data.Profunctor.Strong ((***))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
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

-- | A form that contains two forms.
product :: ∀ p e s1 s2 a1 a2. Semigroup e => Form p e s1 a1 ->
  Form p e s2 a2 -> Form p e (s1 /\ s2) (a1 /\ a2)
product (Form f1) (Form f2) = Form
  { initialState: f1.initialState /\ f2.initialState
  , fromStructure: f1.fromStructure *** f2.fromStructure
  , toStructure: (lift2 (/\)) <$> f1.toStructure <<< fst
                              <*> f2.toStructure <<< snd
  , render: \(a /\ b) ->
      map (map (_ /\ b)) (f1.render a) <>
      map (map (a /\ _)) (f2.render b)
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
