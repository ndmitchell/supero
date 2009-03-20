
module Core.Read(readCore) where

import Language.Haskell.Exts as H
import Compiler.Expr
import Core.Type as C
import Data.Generics.PlateData

readCore :: FilePath -> IO (Prog Core)
readCore file = do
    src <- parseFile file
    case src of
        ParseOk x -> return $ prog x
        x -> error $ show x

prog :: Module -> Prog Core
prog (Module _ _ _ _ _ _ decls) = concatMap f decls
    where
        funs = [var x | FunBind [Match _ x _ _ _ _] <- decls]
    
        f (FunBind [Match _ name vars _ (UnGuardedRhs x) _]) =
            [(var name, lams (map var vars) $ g x)]
        f (PatBind _ name _ (UnGuardedRhs x) _) = [(var name, g x)]
        f x = error $ show ("Core.Read.prog.f",x)

        g (H.Var x) = (if var x `elem` funs then C.Fun else C.Var) (var x)
        g (H.App x y) = C.App (g x) (g y)
        g (H.Case x alts) = C.Case (g x) [(h p, g y) | Alt _ p (UnGuardedAlt y) _ <- alts]
        g (List []) = C.Con "Nil"
        g (InfixApp x z y) = C.App (C.App (i z) (g x)) (g y)
        g (Paren x) = g x
        g (Lit x) = C.Con (prettyPrint x)
        g x = error $ show ("Core.Read.prog.g",x)
        
        h (PList []) = ("Nil",[])
        h (PInfixApp x (Special Cons) y) = ("Cons",[var x,var y])
        h (PApp x xs) = (var x, map var xs)
        h x = error $ show ("Core.Read.prog.h",x)

        i (QConOp (Special Cons)) = C.Con "Cons"
        i (QVarOp x) = g (H.Var x)
        i x = error $ show ("Core.Read.prog.i",x)

        var x = case prettyPrint x of
            "(.)" -> "dot"
            "($)" -> "dol"
            x -> x


