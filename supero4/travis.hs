
import System.Process.Extra
import Control.Exception.Extra

main :: IO ()
main = do
    retry 3 $ system_ "cabal install criterion"
    system_ "supero --compile --test --benchmark --work --quiet"
