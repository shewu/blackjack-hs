module State where

--import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative
import System.Random


newtype StateTransition s a = StateTransition
    { runState :: s -> (a, s)
    }

instance Monad (StateTransition s) where
    --return :: a -> State s a
    return x = StateTransition (\st -> (x, st))

    --(>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) s f = StateTransition (\st -> let (x, st') = runState s st in runState (f x) st')

instance Applicative (StateTransition s) where
    pure = return
    (<*>) = ap

instance Functor (StateTransition s) where
    fmap = liftM

-- primitive example

randomSt :: (RandomGen g, Random a) => StateTransition g a
randomSt = StateTransition random

threeCoins :: StateTransition StdGen (Bool, Bool, Bool)
threeCoins =
    do  a <- randomSt
        b <- randomSt
        c <- randomSt
        return (a, b, c)

-- my stuff

start :: StateTransition Int String
start = StateTransition $ \st -> (show st, st)

inc :: Num a => a -> StateTransition a a
inc st = StateTransition $ \_ -> (st+1, st)

add :: String -> StateTransition Int String
add inp = StateTransition $ \st -> (show $ st+x, st+x)
    where
    x = read inp :: Int

collatz :: String -> StateTransition Int String
collatz "1" = StateTransition $ \st -> ("1", 1)
collatz inp =
    case x `mod` 2 of
        0 -> StateTransition $ \st -> (show $ x `div` 2, 0)
        1 -> StateTransition $ \st -> (show $ 3 * x + 1, 1)
    where
    x = read inp :: Int

-- something like
-- runState (start >>= inc) 0
-- where 0 is starting state
-- what is start???
-- inc is transition

data DoorState = Opened | Closed
    deriving Show
data DoorAction = Open | Close
    deriving Show

type DoorInput = DoorAction
type DoorOutput = String

doorTransition :: DoorInput -> StateTransition DoorState DoorOutput
doorTransition Open = StateTransition $ \st ->
    case st of
        Opened -> ("door is already opened", Opened)
        Closed -> ("door is opened", Opened)
doorTransition Close = StateTransition $ \st -> 
    case st of
        Opened -> ("door is closed", Closed)
        Closed -> ("door is already cloased", Closed)

data TurnstileState = Locked | Unlocked
    deriving Show
data TurnstileInput = Coin | Push
    deriving Show

lockedTransition :: TurnstileInput -> StateTransition TurnstileState String
lockedTransition Coin = StateTransition $ \st -> ("unlock turnstile so customer can push through", Unlocked)
lockedTransition Push = StateTransition $ \st -> ("(none)", Locked)

unlockedTransition :: TurnstileInput -> StateTransition TurnstileState String
unlockedTransition Coin = StateTransition $ \st -> ("(none)", Unlocked)
unlockedTransition Push = StateTransition $ \st -> ("when customer has pushed through lock turnstile", Locked)
