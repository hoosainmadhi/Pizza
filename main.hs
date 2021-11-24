import Pizza
import System.IO

newtype Customer = Customer {name :: String} deriving (Show)

data Order = Order
  { customer :: Customer,
    items :: [BaseProduct]
  }
  deriving (Show)

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
  putStrLn "\n --- MAIN ---"
  putStrLn "1- New Order\n q - Quit"
  line <- getLine
  case line of
    "1" -> do
      menuItem <- displayMenu
      let order = emptyOrder
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
  putStrLn "\n Build Order"
  putStr "1 - Add Menu ItemTo Order\n2 - Return to Menu \nr - Return"
  print menuItem
  line <- getLine
  case line of
    "1" -> do
      let order = addPizzaToOrder orderIn menuItem
      buildOrder menuItem order
    -- "2" -> do

    "r" -> return orderIn
    _ -> return orderIn

displayMenu :: IO BaseProduct
displayMenu = do
  putStrLn "\n Menu"
  putStrLn "1 - Basic\n2 - Super\n3 - Supreme\n4 - SoftDrink\n5 - Breadsticks\n - Return"
  line <- getLine
  case line of
    "1" -> do
      return basicPizza
    "2" -> do
      return superPizza
    "3" -> do
      return supremePizza
    "4" -> do
      return SoftDrink
    "5" -> do
      return Breadsticks
    "r" -> do
      displayMenu
    _ -> displayMenu
