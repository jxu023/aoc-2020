import Debug.Trace (trace)

requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
-- optionalKeys = ["cid"]

type Field = String
type Passport = [Field]

parseKv :: Field -> (String, String)
parseKv field = let (key:val:[]) = splitOn ':' field
                    in (key, val)
                    -- in trace ("field is " ++ show field) (key, val)

parsePassports :: String -> [Passport]
parsePassports = map (words . unwords) . splitOn [] . lines

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = (\(w, ws) -> w:ws) . foldr f ([], [])
    where f c (w, ws) = if c /= x then (c:w, ws)
                                  else ([], w:ws)

hasRequiredFields :: Passport -> Bool
hasRequiredFields fields = let keys = map (fst . parseKv) fields
                           in null $ diffList requiredKeys keys

diffList :: Eq a => [a] -> [a] -> [a]
diffList xs ys = xs >>= \x -> if elem x ys then [] else [x]

numValidPassports :: (Passport -> Bool) -> String -> Int
numValidPassports isValid = length . filter isValid . parsePassports

isNum c = c >= '0' && c <= '9'

validKv :: String -> String -> Bool
validKv "byr" val = (length val == 4) && val >= "1920" && val <= "2002"
validKv "iyr" val = (length val == 4) && val >= "2010" && val <= "2020"
validKv "eyr" val = (length val == 4) && val >= "2020" && val <= "2030"
validKv "hgt" val = let len = length val
                        (numStr, unit) = splitAt (len - 2) val
                        num = (read :: String -> Int) numStr
                        cmValid = unit == "cm" && num >= 150 && num <= 193
                        inValid = unit == "in" && num >= 59 && num <= 76
                        in len > 2 && all isNum numStr && (cmValid || inValid)
validKv "hcl" val = let len = length val
                        (hash, six) = splitAt 1 val
                        isAf09 c = c >= 'a' && c <= 'f' || isNum c
                        in len == 7 && hash == "#" && all isAf09 six
validKv "ecl" val = elem val ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validKv "pid" val = length val == 9 && all isNum val
validKv "cid" _ = True

hasAllValidFields :: Passport -> Bool
hasAllValidFields fields = hasRequiredFields fields && all (uncurry validKv . parseKv) fields
    -- && all ((\x -> trace (if uncurry validKv x then "" else show x) $ uncurry validKv x) . parseKv) fields

main :: IO()
main = do
    exampleContents <- readFile "../example.input"
    problemContents <- readFile "../problem.input"
    print $ numValidPassports hasRequiredFields exampleContents
    print $ numValidPassports hasRequiredFields problemContents

    print $ numValidPassports hasAllValidFields exampleContents
    print $ numValidPassports hasAllValidFields problemContents
