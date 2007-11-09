
module Report(report) where

import Safe
import Data.List
import Data.Maybe


-- The MR sucks!
snub x = sort $ nub x

report :: IO ()
report = do
    src <- readFile "results.txt"
    let res = map (\[a,b,c] -> (a,b,c)) $ map words $ lines src
        (comps,tests,_) = unzip3 res
    comps <- return $ snub comps
    tests <- return $ snub tests

    let f test = "<tr><td>" ++ test ++ "</td>" ++
                 reportTest comps [(a,read c) | (a,b,c) <- res, b == test] ++
                 "</tr>"

    let ans = "<html><head><style type='text/css'>" ++ css ++ "</style></head><body><table>" ++
              "<tr><td></td>" ++ concatMap (tag "td") comps ++ "</tr>" ++
              concatMap f tests ++
              "</table></body></html>"

    writeFile "report.htm" ans


reportTest :: [String] -> [(String,Integer)] -> String
reportTest comps res = concatMap g vals
    where
        (low,high) = (minimum real, maximum real)
        real = catMaybes vals
        vals = map f comps
        
        f c = minimumMay [b | (a,b) <- res, a == c]

        g Nothing = "<td></td>"
        g (Just x) = "<td style='background-color:rgb(255," ++ red ++ "," ++ red ++ ")'>" ++ dp2 val ++ "</td>"
            where
                red = if val < 105 then "255" else
                      show $ round $ (255*) $ (1-) $ fromInteger (x - low) / fromInteger (high - low) 
                val = ((x * 100) `div` low)


dp2 x = reverse whole ++ "." ++ reverse frac
    where (frac,whole) = splitAt 2 $ reverse $ show x



tag x y = "<" ++ x ++ ">" ++ y ++ "</" ++ x ++ ">"


css = unlines
    ["td {border-right:10px solid white; font-size:10pt;}"
    ]
    