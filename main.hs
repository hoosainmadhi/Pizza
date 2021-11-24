import Pizza
import System.IO

newtype Customer = Customer {name :: String} deriving (Show)

data Order = Order
  { customer :: Customer,
    items :: [BaseProduct]
  }
  deriving (Show)

anEmptyOrder :: Order
anEmptyOrder = Order {customer = Customer "no name", items = []}

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
      let order = anEmptyOrder
      finishedOrder <- buildOrder order
      putStrLn "Items Order \n"
      print (items finishedOrder)
      putStrLn "No of Items\n"
      print (length (items finishedOrder))
      print finishedOrder
      main
    _ -> exit

------------------
--  exit :: IO ()
------------------

exit :: IO ()
exit = do putStrLn "exiting Pizza POS"

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

buildOrder :: Order -> IO Order
buildOrder orderIn = do
  putStrLn "\n Build Order"
  putStrLn "1 - Add Pizza To Order\n2 - Customer\n r - Return"
  line <- getLine
  case line of
    "1" -> do
      -- putStrLn "Cool - Choose your Pizza from the List"
      let order = addPizzaToOrder orderIn basicPizza
      buildOrder order
    -- "2" -> do
    --   putStrLn "I see you are new"
    "r" -> return orderIn
    _ -> return orderIn

---------------
--  choosePizza
---------------
