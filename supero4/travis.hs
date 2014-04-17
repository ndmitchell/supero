
import Neil

main :: IO ()
main = do
    retry 3 $ cmd "cabal install criterion"
    cmd "supero --compile --test --benchmark simple peter other digits_of_e2"
