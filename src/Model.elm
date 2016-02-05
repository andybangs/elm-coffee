module Model where

type Ingredient = Coffee | Water | Ratio

type Unit = G | Oz

type alias Component =
  { title : Ingredient
  , value : Float
  , maxValue : Float
  , displayUnit : Unit
  , editing : Bool
  }

type alias Model =
  { coffee : Component
  , water : Component
  , ratio : Component
  , time : Int
  , ticking : Bool
  }

component : Ingredient -> Float -> Float -> Component
component title value maxValue =
  { title = title
  , value = value
  , maxValue = maxValue
  , displayUnit = G
  , editing = False
  }

initialModel : Model
initialModel =
  { coffee = component Coffee 20 100
  , water = component Water 320 1900
  , ratio = component Ratio 16 19
  , time = 0
  , ticking = False
  }
