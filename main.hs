-- User data type
data User = User { userName :: String, userEmail :: String, userPrize :: String }
    deriving (Show)

-- Prize data type
data Prize = Prize { prizeName :: String }
    deriving (Show)

-- Linear Congruential Generator (LCG)
lcg :: Int -> Int -> Int -> Int -> Int
lcg a c m seed = (a * seed + c) `mod` m

-- Function to generate a list of n unique random indices using LCG
getRandomIndices :: Int -> IO [Int]
getRandomIndices n = do
    let seed = 42 -- Choose any seed value
        m = 2^32
        indices = take n $ iterate (lcg 1664525 1013904223 m) seed
    return indices

-- Function to shuffle a list randomly based on LCG
shuffleList :: [a] -> IO [a]
shuffleList xs = do
    indices <- getRandomIndices (length xs)
    return [xs !! (i `mod` length xs) | i <- indices]

-- Function to create a list of 10 users with unique emails and randomly assigned prizes
createRandomUsers :: IO [User]
createRandomUsers = do
    let userNames = ["User1", "User2", "User3", "User4", "User5", "User6", "User7", "User8", "User9", "User10"]
        prizes = ["Prize1", "Prize2", "Prize3", "Prize4", "Prize5", "Prize6", "Prize7", "Prize8", "Prize9", "Prize10"]
    shuffledUserNames <- shuffleList userNames
    shuffledPrizes <- shuffleList prizes
    let uniqueEmails = map (\i -> "user" ++ show i ++ "@example.com") [1..10]
        users = zipWith3 (\name email prize -> User name email prize) shuffledUserNames uniqueEmails shuffledPrizes
    return users

main :: IO ()
main = do
    randomUsers <- createRandomUsers
    mapM_ print randomUsers
