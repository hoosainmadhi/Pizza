module Pizza where

data Topping = Cheese | Pepperoni | Sausage | Onions | Mushrooms deriving (Eq, Show)

data CrustType = Thin | Thick | Regular deriving (Show)

data CrustSize = Small | Medium | Large deriving (Show)

data Products
  = Pizza
      { crustSize :: CrustSize,
        crustType :: CrustType,
        toppings :: [Topping]
      }
  | Breadsticks
  | SoftDrink
  deriving (Show)

-- basic define a pizza instance:
pizza :: Products
pizza = Pizza {crustSize = Medium, crustType = Thin, toppings = [Cheese]}

-- Menu Offerings

basicPizza :: Products
basicPizza = Pizza {crustSize = Medium, crustType = Thin, toppings = [Cheese]}

superPizza :: Products
superPizza = Pizza {crustSize = Large, crustType = Thin, toppings = [Cheese, Onions, Mushrooms]}

supremePizza :: Products
supremePizza = Pizza {crustSize = Large, crustType = Thin, toppings = [Cheese, Onions, Mushrooms, Sausage, Pepperoni]}

-- now we want to add a single topping to the Pizza
addTopping :: Products -> [Topping] -> Products
addTopping p ts =
  Pizza {crustSize = crustSize p, crustType = crustType p, toppings = newToppings}
  where
    newToppings = ts ++ toppings p