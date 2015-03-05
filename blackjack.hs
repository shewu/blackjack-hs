module Blackjack where

--import State

import Control.Applicative
import Control.Monad
import Control.Monad.State (liftIO, foldM, StateT, runStateT, get, put, State, state, runState)
import qualified Data.Map as M
import Data.Time.LocalTime
import Data.Traversable
import System.Random


data Suit = Spade | Heart | Diamond | Club deriving (Eq, Show)
data Value
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    deriving (Enum, Eq, Show)

data Card = Card
    { suit :: Suit
    , value :: Value
    } deriving (Eq, Show)

type Hand = [Card]
type Deck = [Card]

allCards :: Deck
allCards = [Card s v | v <- [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
                     , s <- [Spade, Heart, Diamond, Club]
                     ]

data Turn = Dealer | Player deriving Show

data GameState = GameState
    { dealerHand :: Hand
    , playerHand :: [Hand]
    , deck :: Deck
    , whoseTurn :: Turn
    , boughtInsurance :: Bool
    } deriving Show

data GameAction
    = Hit
    | Stay
    | DoubleDown
    | Split
    | Fold
    deriving (Eq, Read, Show)

data Outcome
    = Win
    | Lose
    | Tie
    deriving (Eq, Show)

---------------
-- functions --
---------------

io :: IO a -> StateT GameState IO a
io = liftIO

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt p = state $ randomR p

shuffledSingleDeck :: State StdGen Deck
--shuffledSingleDeck = fisherYates allCards
shuffledSingleDeck = return allCards

-- XXX this is actually foldM
foldlM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM _ b [] = return b
foldlM f b (x:xs) = (f b x) >>= (\nextB -> foldlM f nextB xs)

fisherYates :: [a] -> State StdGen [a]
fisherYates [] = return []
fisherYates (x:xs) =
    M.elems <$> foldM fisherYatesStep (initial x) (numerate xs)
    where
    numerate = zip [1..]
    initial k = M.singleton 0 k

    fisherYatesStep :: M.Map Int a -> (Int, a) -> State StdGen (M.Map Int a)
    fisherYatesStep m (i, x) = (\j -> M.insert j x . M.insert i (m M.! j) $ m) <$> randomRSt (0, i)

hasValueTen :: Value -> Bool
hasValueTen v = 10 <= (fromEnum v) + 1

hasBlackjack :: Hand -> Bool
hasBlackjack ((Card _ Ace):(Card _ y):[]) = hasValueTen y
hasBlackjack ((Card _ x):(Card _ Ace):[]) = hasValueTen x
hasBlackjack _ = False

handValue :: Hand -> Int
handValue [] = 0
handValue ((Card _ Ace):h') = if hi <= 21 then hi else lo
    where
    hv = handValue h'
    lo = 1 + hv
    hi = 11 + hv
handValue ((Card _ f):h')
    | hasValueTen f = 10 + (handValue h')
    | otherwise = 1 + (fromEnum f) + (handValue h')

didExplode :: Hand -> Bool
didExplode h = (handValue h) > 21

-- cannot double down after split
canDoubleDown :: [Hand] -> Bool
canDoubleDown (((Card _ _):(Card _ _):[]):[]) = True
canDoubleDown _ = False

canSplit :: Hand -> Bool
canSplit ((Card _ v1):(Card _ v2):[]) = v1 == v2
canSplit _ = False

nextTurn :: Turn -> Turn
nextTurn Dealer = Player
nextTurn Player = Dealer

gameSt :: State StdGen GameState
gameSt = (\d -> GameState [] [[]] d Dealer False) <$> shuffledSingleDeck

compareHands :: Hand -> [Hand] -> [Outcome]
compareHands dh phs = let dhv = handValue dh in
    map (\h -> let hv = handValue h in
        if dhv > hv
            then Lose
            else if dhv < hv
                then Win
                else Tie
    ) phs

drawCard :: StateT GameState IO (Maybe Card)
drawCard = do
    (GameState dh ph d t ins) <- get
    case d of
        (c:cs) -> (put $ GameState dh ph cs t ins) >> (return $ Just c)
        [] -> return Nothing

giveDealer :: StateT GameState IO (Maybe Card)
giveDealer = do
    (GameState dh ph d t ins) <- get
    case d of
        (c:cs) -> (put $ GameState (c:dh) ph cs t ins) >> (return $ Just c)
        [] -> return Nothing

playDealer :: StateT GameState IO ()
playDealer = do
    c <- giveDealer
    io $ putStrLn ("Dealer got a " ++ (show c))
    (GameState dh ph d t ins) <- get
    case dealerStop dh of
        True -> case didExplode dh of
            True -> do io $ putStrLn "Dealer exploded! You win."
                       return ()
            False -> do io . putStrLn $ "Dealer's hand is " ++ (show dh)
                        return ()  -- TODO compare hands
        False -> playDealer
    where
    dealerStop h = (handValue h) >= 17

-- XXX this is just for one hand
dealPlayer :: StateT GameState IO (Maybe Card)
dealPlayer = do
    (GameState dh ph d t ins) <- get
    case d of
        (c:cs) -> case ph of
            [] -> return Nothing
            (h:hs) -> (put $ GameState dh ((c:h):hs) cs t ins) >> (return $ Just c)
        [] -> return Nothing

playPlayer :: StateT GameState IO ()
playPlayer = do
    (GameState _ ph _ _ _) <- get
    ph' <- traverse playHand ph  --XXX still not sure how to append to ph
    (GameState dh _ d t ins) <- get
    put $ GameState dh ph' d t ins
    case all (== []) ph' of
        True -> do
            io $ putStrLn "Player lost. Dealer wins!"
        False -> do
            io $ putStrLn "Dealer's turn."
            playDealer
    where
    playHand :: Hand -> StateT GameState IO Hand
    playHand h = do
        io . putStrLn $ "Playing hand " ++ (show h)
        io $ putStr "Your move [Hit Stay DoubleDown Split Fold]> "
        -- XXX read sucks. it can throw exceptions, which are unidiomatic.
        (read <$> io getLine) >>= processInput
        where
        processInput :: GameAction -> StateT GameState IO Hand
        processInput ga = case ga of
            Hit -> do
                mc <- drawCard
                case mc of
                    Just c -> do
                        io . putStrLn $ "Player got a " ++ (show c)
                        let h' = c:h
                        case didExplode h' of
                            True -> do
                                io . putStrLn $ "The current hand " ++ (show h') ++ " loses."
                                return []
                            False -> playHand h'
                    Nothing -> do
                        io . putStrLn $ "Deck ran out of cards!"
                        return h
            Stay -> return h
            DoubleDown -> do
                (GameState _ ph _ _ _) <- get
                case canDoubleDown ph of
                    True -> do
                        h' <- processInput Hit
                        if h' == []
                            then return []
                            else processInput Stay
                    False -> do
                        io $ putStrLn "Can't double down"
                        playHand h
            Split -> do --TODO
                (GameState _ ph _ _ _) <- get
                case canSplit h of
                    True -> do
                        Just c1 <- drawCard
                        Just c2 <- drawCard
                        (GameState dh _ d t ins) <- get
                        let sc = head h  -- despite head, this is safe becaause of canSplit
                        let ph' = ph ++ [(sc:c2:[])]
                        put $ GameState dh ph' d t ins
                        return (sc:c1:[])
                    False -> do
                        io $ putStrLn "Can't split"
                        playHand h
            Fold -> do
                io . putStrLn $ "The current hand " ++ (show h) ++ " loses."
                return []
            --_ -> (io . putStrLn $ "Invalid input " ++ (show ga)) >> playHand h

play :: StateT GameState IO ()
play = do
    c <- giveDealer
    io . putStrLn $ "Dealer got a ???"
    needsInsurance c
    -- XXX really should check whether the deck is out of cards
    dealPlayer >>= (\(Just c) -> io . putStrLn $ "Player got a " ++ (show c))
    giveDealer >>= (\(Just c) -> io . putStrLn $ "Dealer got a " ++ (show c))
    dealPlayer >>= (\(Just c) -> io . putStrLn $ "Player got a " ++ (show c))
    playPlayer
    where
    needsInsurance :: Maybe Card -> StateT GameState IO ()
    needsInsurance mc@(Just (Card _ Ace)) = do
        io $ putStr "Do you want insurance? (y/n)> "
        inp <- io getLine
        case inp of
            "y" -> do (GameState dh ph cs t ins) <- get
                      put $ GameState dh ph cs t True
                      return ()
            "n" -> return ()
            _ -> needsInsurance mc
    needsInsurance _ = return ()

main :: IO ()
main = do
    putStrLn "Welcome to blackjack"
    runStateT play startingGameState >> return ()
    where
    (startingGameState, _) = runState gameSt (mkStdGen 20934)
