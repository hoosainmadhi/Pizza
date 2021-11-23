import Graphics.UI.GLUT (exit)
import Pizza (BaseProduct)
import System.IO

data Customer = Customer {name :: String} deriving (Show)

data Order = Order
  { customer :: Customer,
    pizzas :: [BaseProduct]
  }
  deriving (Show)

createEmptyOrder :: Order
createEmptyOrder = Order {customer = (Customer "no name"), pizzas = []}

---------------
--  MAIN
---------------

main = do
  putStrLn "\n --- MAIN ---"
  putStrLn "1- New Order\n q - Quit"
  line <- getLine
  case line of
    "1" -> do
      print "New Order"
    _ -> print "quitting.."
