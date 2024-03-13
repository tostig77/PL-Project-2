-- arithmetic operations -- 

binarySum :: [Int] -> [Int] -> [Int]
binarySum x y = snd (binarySum' x y)
  where
    binarySum' :: [Int] -> [Int] -> (Int, [Int])
    binarySum' [] [] = (0, [])
    binarySum' (x:xs) (y:ys) = 
        let (carry, rest) = binarySum' xs ys
            sum' = bitXOR (bitXOR x y) carry
            carry' = bitOR (bitAND x y) (bitAND carry (bitXOR x y))
        in (carry', sum' : rest)

twosComplement :: [Int] -> [Int]
twosComplement x = binarySum (binaryNOT x) [0,0,0,0,0,0,0,1]

binarySubtract :: [Int] -> [Int] -> [Int]
binarySubtract x y = binarySum x (twosComplement y)



-- logical operations -- 

binaryNOT :: [Int] -> [Int]
binaryNOT [] = [] -- Base case: empty list
binaryNOT (x:xs) = bitNOT x : binaryNOT xs 

binaryAND :: [Int] -> [Int] -> [Int]
binaryAND [] _ = [] 
binaryAND _ [] = [] 
binaryAND (x:xs) (y:ys) = (bitAND x y) : binaryAND xs ys 

binaryOR :: [Int] -> [Int] -> [Int]
binaryOR [] ys = ys 
binaryOR xs [] = xs 
binaryOR (x:xs) (y:ys) = (bitOR x y) : binaryOR xs ys 

binaryXOR :: [Int] -> [Int] -> [Int]
binaryXOR [] ys = ys 
binaryXOR xs [] = xs 
binaryXOR (x:xs) (y:ys) = (bitXOR x y) : binaryXOR xs ys 



-- logical operators on bits -- 

bitAND :: Int -> Int -> Int
bitAND 1 1 = 1
bitAND _ _ = 0

bitOR :: Int -> Int -> Int
bitOR 0 0 = 0
bitOR _ _ = 1

bitNOT :: Int -> Int
bitNOT 0 = 1
bitNOT 1 = 0
bitNOT _ = error "Invalid bit value, must be 0 or 1"

bitXOR :: Int -> Int -> Int
bitXOR 0 0 = 0
bitXOR 1 1 = 0
bitXOR _ _ = 1



-- decimal/binary/hexadecimal conversions -- 

decimalToBinary:: Int -> [Int]
decimalToBinary n
  | n == 0 = replicate 8 0 
  | n > 0 =  padToEightBits (reverse (decimalToBinary' n))
  | otherwise = twosComplement (padToEightBits (reverse (decimalToBinary' (-n))))
    where 
    decimalToBinary':: Int -> [Int]
    decimalToBinary' 0 = []
    decimalToBinary' n = let (q,r) = n `divMod` 2 in r : decimalToBinary' q

    padToEightBits :: [Int] -> [Int]
    padToEightBits bits = replicate (8 - length bits) 0 ++ bits

binaryToDecimal :: [Int] -> Int
binaryToDecimal x 
    | x == [1,0,0,0,0,0,0,0] = -(128)
    | head x == 1 = - (binaryToDecimal (binaryNOT (binarySubtract x [0,0,0,0,0,0,0,1])))
    | otherwise = binaryToDecimal' x 0
    where 
        binaryToDecimal' [] num = num
        binaryToDecimal' (x:xs) num = binaryToDecimal' xs ((num * 2) + x)


hexDigitToBinary :: Char -> [Int]
hexDigitToBinary '0' = [0,0,0,0]
hexDigitToBinary '1' = [0,0,0,1]
hexDigitToBinary '2' = [0,0,1,0]
hexDigitToBinary '3' = [0,0,1,1]
hexDigitToBinary '4' = [0,1,0,0]
hexDigitToBinary '5' = [0,1,0,1]
hexDigitToBinary '6' = [0,1,1,0]
hexDigitToBinary '7' = [0,1,1,1]
hexDigitToBinary '8' = [1,0,0,0]
hexDigitToBinary '9' = [1,0,0,1]
hexDigitToBinary 'A' = [1,0,1,0]
hexDigitToBinary 'B' = [1,0,1,1]
hexDigitToBinary 'C' = [1,1,0,0]
hexDigitToBinary 'D' = [1,1,0,1]
hexDigitToBinary 'E' = [1,1,1,0]
hexDigitToBinary 'F' = [1,1,1,1]
hexDigitToBinary _   = error "Invalid hexadecimal digit"

hexToBinary :: String -> [Int]
hexToBinary ('0':'x':xs) = hexToBinary xs  
hexToBinary x = concatMap hexDigitToBinary x

binaryDigitToHex :: [Int] -> Char
binaryDigitToHex [0,0,0,0] = '0'
binaryDigitToHex [0,0,0,1] = '1'
binaryDigitToHex [0,0,1,0] = '2'
binaryDigitToHex [0,0,1,1] = '3'
binaryDigitToHex [0,1,0,0] = '4'
binaryDigitToHex [0,1,0,1] = '5'
binaryDigitToHex [0,1,1,0] = '6'
binaryDigitToHex [0,1,1,1] = '7'
binaryDigitToHex [1,0,0,0] = '8'
binaryDigitToHex [1,0,0,1] = '9'
binaryDigitToHex [1,0,1,0] = 'A'
binaryDigitToHex [1,0,1,1] = 'B'
binaryDigitToHex [1,1,0,0] = 'C'
binaryDigitToHex [1,1,0,1] = 'D'
binaryDigitToHex [1,1,1,0] = 'E'
binaryDigitToHex [1,1,1,1] = 'F'
binaryDigitToHex xs = error $ "Invalid binary digit: " ++ show xs


binaryToHex :: [Int] -> String
binaryToHex []           = "0x0"
binaryToHex binDigits    = "0x" ++ binaryToHex' binDigits
    where 
    binaryToHex' :: [Int] -> String
    binaryToHex' []           = ""
    binaryToHex' binDigits    = let (chunk, rest) = splitAt 4 binDigits
                                 in binaryDigitToHex chunk : binaryToHex' rest



-- helper functions -- 

logicalOperation :: String -> String -> String -> String
logicalOperation xHex op yHex =
    let x = hexToBinary xHex
        y = hexToBinary yHex
        result = case op of
            "AND" -> binaryToHex $ binaryAND x y
            "OR"  -> binaryToHex $ binaryOR x y
            "XOR" -> binaryToHex $ binaryXOR x y
            _     -> error "Invalid operator"
    in result

logicalUnaryOperation :: String -> String -> String
logicalUnaryOperation op xHex =
    let x = hexToBinary xHex
        result = case op of
            "NOT" -> binaryToHex $ binaryNOT x
            _   -> error "Invalid operator"
    in result

arithmeticOperation :: Int -> String -> Int -> Int
arithmeticOperation x op y =
    let xBin = decimalToBinary x
        yBin = decimalToBinary y
        resultBin = case op of
            "ADD" -> binaryToDecimal $ binarySum xBin yBin
            "SUB" -> binaryToDecimal $ binarySubtract xBin yBin
            _     -> error "Invalid operator"
    in resultBin




main :: IO ()
main = do
    let result1 = logicalUnaryOperation "NOT" "0xA5"
    let result2 = logicalOperation "0xF9" "AND" "0x9F"
    let result3 = logicalOperation "0x01"  "OR"  "0x11"
    let result4 = logicalOperation "0xFF" "XOR" "0x88"
    let result5 = arithmeticOperation 121 "ADD" 6
    let result6 = arithmeticOperation 127 "ADD" (-(6))
    let result7 = arithmeticOperation 5 "SUB" 5
    let result8 = arithmeticOperation 5 "SUB" (-(5))
    let result9 = arithmeticOperation 64 "ADD" 64
    let result10 = arithmeticOperation 10 "SUB" 11
    let result11 = arithmeticOperation 128 "ADD" 1

    putStrLn $ "NOT 0xA5: " ++ show result1
    putStrLn $ "0xF9 AND 0x9F: " ++ show result2
    putStrLn $ "0x01  OR  0x11: " ++ show result3
    putStrLn $ "0xFF XOR 0x88: " ++ show result4
    putStrLn $ "121 ADD 6: " ++ show result5
    putStrLn $ "127 ADD -6: " ++ show result6
    putStrLn $ "5 SUB 5: " ++ show result7
    putStrLn $ "5 SUB -5: " ++ show result8
    putStrLn $ "64 ADD 64: " ++ show result9
    putStrLn $ "10 SUB 11: " ++ show result10
    putStrLn $ "128 ADD 1: " ++ show result11


