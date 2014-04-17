
import Neil

main :: IO ()
main = do
    retry 3 $ cmd "cabal install criterion"
    cmd "supero --compile --test --benchmark mapmap rev prims mapid iterate index digits_of_e2"
