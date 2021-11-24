module Pizza where

data Topping = Cheese | Pepperoni | Sausage | Onions | Mushrooms deriving (Eq, Show)

data CrustType = Thin | Thick | Regular deriving (Show)

data CrustSize = Small | Medium | Large deriving (Show)

data BaseProduct
  = Pizza
      { crustSize :: CrustSize,
        crustType :: CrustType,
        toppings :: [Topping]
      }
  | Breadsticks
  | SoftDrink
  deriving (Show)

-- basic define a pizza instance:
pizza :: BaseProduct
pizza = Pizza {crustSize = Medium, crustType = Thin, toppings = [Cheese]}

-- now we want to add a single topping to the Pizza
addTopping :: BaseProduct -> [Topping] -> BaseProduct
addTopping p ts =
  Pizza {crustSize = crustSize p, crustType = crustType p, toppings = newToppings}
  where
    newToppings = ts ++ toppings p