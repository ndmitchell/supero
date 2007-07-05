
module Generate(generate) where

import Yhc.Core hiding (primName)
import Data.List
import Data.Char


-- | Generate a Haskell program from a string
generate :: FilePath -> Core -> IO ()
generate output core = do
    src <- readFile "library/Prefix.hs"
    let body = unlines . tail . lines . show . fixup
    writeFile output (src ++ body core)


-- if a name is from the Prelude, drop it
-- if it is generated, encode it properly
fixup :: Core -> Core
fixup core = core{coreDatas = concatMap fData (coreDatas core)
                 ,coreFuncs = concatMap fFunc (coreFuncs core)}
    where
        fData (CoreData name tys ctrs)
            | "Prelude." `isPrefixOf` name = []
            | otherwise = [CoreData (upperName name) tys (map fCtor ctrs)]

        fCtor (CoreCtor name fields) = CoreCtor (upperName name) fields

        fFunc (CorePrim{}) = []
        fFunc (CoreFunc name args body) = [CoreFunc (lowerName name) (map lowerName args) (mapUnderCore fExpr body)]
        
        fExpr (CoreFun x) = CoreFun (lowerName x)
        fExpr (CoreCon x) = CoreCon (upperName x)
        fExpr (CoreVar x) = CoreVar (lowerName x)
        fExpr (CoreLet bind x) = CoreLet [(lowerName a, b) | (a,b) <- bind] x

        fExpr (CoreInt x) = CoreApp (CoreFun "int_") [CoreInt x]
        fExpr (CoreChr x) = CoreApp (CoreFun "chr_") [CoreChr x]
        fExpr (CoreStr x) = CoreApp (CoreFun "str_") [CoreStr x]
        
        fExpr (CoreCase on [(CoreVar lhs,rhs)]) =
            CoreLet [(lhs,on)] (CoreApp (CoreFun "seq") [CoreVar lhs,rhs])

        fExpr x = x



rep from to x = if x == from then to else x


primName :: String -> String
primName x = "p_" ++ fixName x


upperName :: String -> String
upperName x | "Prelude." `isPrefixOf` x = drop 8 x
            | otherwise = fixName x

lowerName :: String -> String
lowerName x | x == "main" = "main_generated"
            | x == "Prelude.." = "o"
            | otherwise = case fixName x of
                              (c:cs) | isAlpha c -> toLower c : cs
                              cs -> 'l' : cs


escapes = [">gt","<lt","!ex","=eq","+p","$d",":c","[ob","]sb","-h","/fs","|vb"]
boring = ["Prelude","YHC","Internal"]


fixName :: String -> String
fixName = map (rep ' ' '_') . unwords . f . words . map (rep '.' ' ')
    where
        f (x:xs) = map (concatMap g) $ x : filter (`notElem` boring) xs

        g x = case [ys | y:ys <- escapes, y == x] of
                   (y:_) -> '\'' : y
                   _ -> [x]
