import Pizza
import System.IO

newtype Customer = Customer {name :: String} deriving (Show)

data Order = Order
  { customer :: Customer,
    items :: [Products]
  }
  deriving (Show)

data MenuAction = Choice Products | Back

emptyOrder :: Order
emptyOrder = Order {customer = Customer "no name", items = []}

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
      Choice menuItem <- displayMenu
      let order = emptyOrder
      finishedOrder <- buildOrder order menuItem
      printOrder finishedOrder
      main
    _ -> exit

------------------------
--  addPizzaToOrder
------------------------
addPizzaToOrder :: Order -> Products -> Order
addPizzaToOrder order item =
  Order {customer = customer order, items = itemList}
  where
    itemList = item : items order -- add  pizzas to list

------------------------
--  buildOrder
------------------------

buildOrder :: Order -> Products -> IO Order
buildOrder orderIn menuItem = do
  putStrLn "\n"
  print menuItem
  putStrLn "1 - Add and Choose Another Item\nr - print order"
  
  line <- getLine
  case line of
    "1" -> do
      Choice menuItem <- displayMenu
      let order = addPizzaToOrder orderIn menuItem
      buildOrder order menuItem
      
    "r" -> return orderIn
    _ -> return orderIn

------------------
--  displayMenu ()
------------------

displayMenu :: IO MenuAction
displayMenu = do
  putStrLn "\n--- Choose and Item ---"
  putStrLn "1 - Basic\n2 - Super\n3 - Supreme\n4 - SoftDrink\n5 - Breadsticks\nr - Return"
  line <- getLine
  case line of
    "1" -> do
      return $ Choice basicPizza
    "2" -> do
      return $ Choice superPizza
    "3" -> do
      return $ Choice supremePizza
    "4" -> do
      return $ Choice SoftDrink
    "5" -> do
      return $ Choice Breadsticks
    "r" -> do
      return Back
    _ -> return Back

------------------
--  orderToString
------------------

orderToString :: Order -> String
orderToString Order {customer = c, items = p} =
  "Your Order is as follows" ++ "Customer: " ++ show c ++ ", Pizzas: " ++ show p

------------------
--  printOrder
------------------

printOrder :: Order -> IO ()
printOrder order = do putStrLn (orderToString order)

------------------
--  exit :: IO ()
------------------

exit :: IO ()
exit = do putStrLn "exiting Pizza POS"