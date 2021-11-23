import Pizza (BaseProduct)
import System.IO

data Customer = Customer {name :: String} deriving (Show)

data Order = Order
  { customer :: Customer,
    pizzas :: [BaseProduct]
  }
  deriving (Show)

createEmptyOrder :: Order
createEmptyOrder = Order {customer = Customer "foo", pizzas = []}

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
      let order = createEmptyOrder
      finishedOrder <- builderOrder order


      main
    _ -> exit

---------------
--  MAIN
---------------

exit :: IO ()
exit = do putStrLn "exiting Pizza POS"

---------------
--  buildOrder
---------------

builderOrder orderIn = do
  putStrLn "\n Build Order"
  putStrLn "1 - New Pizza\n2 - Customer\n r - Return"
  line <- getLine 
  case line of 
    "1" -> do
      putStrLn "Cools lets make your Pizza"
      
    "2" -> do
      putStrLn "I see you are new"
    "r" -> return orderIn
    _   -> return orderIn