module SayNames.Plugin (plugin) where
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  return ({-todo ++ -} [CoreDoPluginPass "Say name" pass] ++ todo)

pass :: ModGuts -> CoreM ModGuts
pass = bindsOnlyPass (mapM printBind)
  where printBind :: CoreBind -> CoreM CoreBind
        printBind bndr@(NonRec b (Var v)) = do
          flgs <- getDynFlags
          putMsgS $ showSDoc flgs $ ppr $ unfoldingInfo $ idInfo v
          -- res <- lookupId "GHC.Base.$"
          putMsgS $ "Binding is " ++ showSDoc flgs (ppr bndr)
          return bndr
{-
          printBind _ = do
          flgs <- getDynFlags
          putMsgS $ "Non-recursive binding named " ++ showSDoc flgs (ppr bndr) --(ppr b)
          return bndr 
-}
        printBind bndr = return bndr
