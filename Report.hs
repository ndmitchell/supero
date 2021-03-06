
module Report(report, reportFile) where

import Safe
import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.Info


-- The MR sucks!
snub x = sort $ nub x

reportFile :: FilePath
reportFile = "results." ++ os ++ ".txt"

report :: IO ()
report = do
    b <- doesFileExist reportFile
    src <- if not b then return "" else readFile reportFile
    let res = map (\[a,b,c] -> (a,b,c)) $ map words $ lines src
        (comps,tests,_) = unzip3 res
    comps <- return $ snub comps
    tests <- return $ snub tests

    let f test = "<tr><td>" ++ test ++ "</td>" ++
                 reportTest comps [(a,c2) | (a,b,c) <- res, b == test, let c2 = read c, c2 > 0] ++
                 "</tr>"

    let ans = "<html><head><title>Supero Performance Results</title>" ++
              "<style type='text/css'>" ++ css ++ "</style></head><body><table>" ++
              "<tr><td></td>" ++ concatMap (tag "td") comps ++ "</tr>" ++
              concatMap f tests ++
              "</table></body></html>"

    let reportFile = "report." ++ os ++ ".htm"
        reportWeb = "/usr/ndm/web/temp/"
    writeFile reportFile ans
    b <- doesDirectoryExist reportWeb
    when b $ copyFile reportFile (reportWeb </> reportFile)


reportTest :: [String] -> [(String,Integer)] -> String
reportTest comps res = concatMap g vals
    where
        (low,high) = (minimum real, maximum real)
        real = catMaybes vals
        vals = map f comps
        
        f c = minimumMay [b | (a,b) <- res, a == c]

        g Nothing = "<td></td>"
        g (Just x) = "<td style='text-align:right;background-color:rgb(255," ++ red ++ "," ++ red ++ ")'>" ++ dp2 val ++ "</td>"
            where
                red = if val < 105 then "255" else
                      show $ round $ (255*) $ (1-) $ fromInteger (x - low) / fromInteger (high - low) 
                val = ((x * 100) `div` low)


dp2 x = reverse whole ++ "." ++ reverse frac
    where (frac,whole) = splitAt 2 $ reverse $ show x



tag x y = "<" ++ x ++ ">" ++ y ++ "</" ++ x ++ ">"


css = unlines
    ["td {border-right:10px solid white; font-size:10pt; font-family:sans-serif;}"
    ]
    