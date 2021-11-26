import Pizza
import System.IO

newtype Customer = Customer {name :: String} deriving (Show)

data Order = Order
  { customer :: Customer,
    items :: [BaseProduct]
  }
  deriving (Show)

orderToString :: Order -> String
orderToString Order {customer = c, items = p} =
  "Your Order is as follows-> " ++ "Customer: " ++ show c ++ ", Pizzas: " ++ show p

printOrder :: Order -> IO ()
printOrder order = do putStrLn (orderToString order)

emptyOrder :: Order
emptyOrder = Order {customer = Customer "no name", items = []}

-- Menu Offerings
basicPizza :: BaseProduct
basicPizza = Pizza {crustSize = Medium, crustType = Thin, toppings = [Cheese]}

superPizza :: BaseProduct
superPizza = Pizza {crustSize = Large, crustType = Thin, toppings = [Cheese, Onions, Mushrooms]}

supremePizza :: BaseProduct
supremePizza = Pizza {crustSize = Large, crustType = Thin, toppings = [Cheese, Onions, Mushrooms, Sausage, Pepperoni]}

---------------
--  MAIN
---------------

main :: IO ()
main = do
  putStrLn "\n --- Welcome to Hazkell's Pizzas ---"
  putStrLn "1- View Menu\nq - Quit"
  line <- getLine
  case line of
    "1" -> do
      menuItem <- displayMenu
      -- putStrLn "Your Order:"
      -- printOrder emptyOrder
      -- menuItem <- displayMenu
      let order = addPizzaToOrder emptyOrder menuItem
      -- printOrder order
      -- displayMenu
      -- let order = emptyOrder
      finishedOrder <- buildOrder menuItem order
      print finishedOrder
      main
    _ -> exit

------------------
--  exit :: IO ()
------------------

exit :: IO ()
exit = do putStrLn "exiting Pizza POS"

------------------
--  mainMenu :: IO ()
------------------
mainMenu :: IO ()
mainMenu = do main

------------------------
--  addPizzaToOrder
------------------------
addPizzaToOrder :: Order -> BaseProduct -> Order
addPizzaToOrder order item =
  Order {customer = customer order, items = itemList}
  where
    itemList = item : items order -- add  pizzas to list

---------------
--  buildOrder or
--  calls addPizzaToOder which adds pizza to pizzas list
-- start with empty list (anEmptyOrder)
---------------

buildOrder :: BaseProduct -> Order -> IO Order
buildOrder menuItem orderIn = do
  putStrLn "\n"
  putStrLn "1 - Add Another Item To Order\n2 - Return to Menu \nr - Return"
  print menuItem
  line <- getLine
  case line of
    "1" -> do
      menuItem <- displayMenu
      let order = addPizzaToOrder orderIn menuItem
      buildOrder menuItem order
    "r" -> return orderIn
    _ -> return orderIn

displayMenu :: IO BaseProduct
displayMenu = do
  putStrLn "\n--- Choose and Item ---"
  putStrLn "1 - Basic\n2 - Super\n3 - Supreme\n4 - SoftDrink\n5 - Breadsticks\nr - Return"
  line <- getLine
  case line of
    "1" -> do
      return basicPizza
    -- displayMenu
    "2" -> do
      return superPizza
    -- displayMenu
    "3" -> do
      return supremePizza
    -- displayMenu
    "4" -> do
      return SoftDrink
    -- displayMenu
    "5" -> do
      return Breadsticks
    -- displayMenu
    "r" -> do
      displayMenu
    _ -> displayMenu

-- orderToString :: Order -> String
-- orderToString (Order {customer = c , pizzas = p}) =
--   "Customer:" ++ show c ++ ", Pizzas: " ++  show p

takeOrder :: IO BaseProduct
takeOrder = displayMenu