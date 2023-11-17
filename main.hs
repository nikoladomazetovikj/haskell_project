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
    let userNames = ["John", "Jane", "Anna", "Bob", "Tom", "Sabrina", "Mike", "Serena", "Itan", "Mark"]
        prizeNames = ["50$", "100$", "150$", "200$", "300$", "500$", "700$", "1000$", "1500", "2500$", "5000$", "10000$", "100000$", "500000$", "1000000$"]
    shuffledUserNames <- shuffleList userNames
    shuffledPrizeNames <- shuffleList prizeNames
    let uniqueEmails = map (\i -> "user" ++ show i ++ "@example.com") [1..10]
        users = zipWith3 (\name email prize -> User name email prize) shuffledUserNames uniqueEmails shuffledPrizeNames
    return users

main :: IO ()
main = do
    randomUsers <- createRandomUsers
    mapM_ print randomUsers
