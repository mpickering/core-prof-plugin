-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module CoreProf (plugin) where
import GhcPlugins
import CostCentre
import CostCentreState
import Debug.Trace as T
import FastString (fsLit)
-- import Control.Monad.ST
-- import Data.STRef
import Control.Monad.State
import DynFlags


plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return (todo ++ [CoreDoPluginPass "CoreProf" coreProf])


coreProf :: ModGuts -> CoreM ModGuts
coreProf = bindsOnlyPass coreProf'

coreProf' :: CoreProgram -> CoreM CoreProgram
coreProf' cp = do
  mod <- getModule
  dflags <- getDynFlags
  if gopt Opt_SccProfilingOn dflags then
    return $ wrapSyntaxTicks (gopt Opt_ProfCountEntries dflags) mod cp
  else
    return $ cp
  -- return $ map (addTick mod) cp

-- Take all syntax ticks and wrap them with cost centre ticks.
wrapSyntaxTicks :: Bool -> Module -> CoreProgram -> CoreProgram
wrapSyntaxTicks count mod cp = evalState (mapM bind cp) newCostCentreState
  where
    bind :: Bind Id -> State CostCentreState (Bind Id)
    bind (NonRec b r) = (expr r) >>= return . NonRec b
    bind (Rec rs) = Rec <$> mapM (\(b, r) -> expr r >>= return . (b,)) rs

    expr :: Expr Id -> State CostCentreState (Expr Id)
    expr exp = case exp of
      App exp' arg         -> App <$> (expr exp') <*> (expr arg)
      Lam b exp'           -> Lam b <$> (expr exp')
      Let bind' exp'       -> Let <$> (bind bind') <*> (expr exp')
      Case exp' b typ alts -> (\e -> Case e b typ) <$> (expr exp') <*> (mapM alt alts)
      Cast exp' coerce     -> (\e -> Cast e coerce) <$> (expr exp')
      Tick tickish exp'    -> tick tickish exp'
      e -> return e
      -- where e = expr exp'

    alt :: Alt Id -> State CostCentreState (Alt Id)
    alt (altcon, bs, exp) = (altcon, bs,) <$> (expr exp)

    tick :: Tickish Id -> Expr Id -> State CostCentreState (Expr Id)
    tick tickish exp
      | Cast{} <- stripTicksTopE (const True) exp = do
          pprTraceM "Ignoring" (ppr exp)
          return $ Tick tickish exp
      | otherwise = case tickish of
        -- Causes a gcc crash?
        -- t@(SourceNote ss sn) -> Tick t (Tick (ProfNote (mkAllCafsCC mod (RealSrcSpan ss)) True True) (expr exp'))
        -- t@(SourceNote ss sn) -> Tick t (Tick (ProfNote (mkAutoCC undefined mod) True True) (expr exp'))
        t@(SourceNote ss sn) -> do
          pprTraceM "Inserting" (text sn $$ ppr exp)
          let name = (fsLit (sn ++ "_core_prof"))
          index <- state (getCCIndex name)
          let cc = ProfNote (mkUserCC name mod (RealSrcSpan ss) (ExprCC index)) count True
          return $ mkTick t (mkTick cc exp)--(expr exp'))

        -- t@(SourceNote ss sn) -> Tick (T.traceShowId t) (expr exp')
        t -> return $ Tick t exp--(expr exp')
        -- Tick (T.traceShowId tickish) (expr exp')

instance Show (Tickish Id) where
  show (ProfNote _ _ _) = "ProfNote"
  show (HpcTick _ _)    = "HpcTick"
  show (Breakpoint _ _) = "Breakpoint"
  show (SourceNote ss sn) = "SourceNote: " ++ show ss ++ ", " ++ sn
  -- show t ->

addTick :: Module -> Bind CoreBndr -> Bind CoreBndr
addTick mod (NonRec b r) = NonRec b (Tick tick r)
  where
    tick = ProfNote (mkAutoCC b mod) True True
addTick mod (Rec rs) = Rec (map go rs)
  where
    go (b, e) =
      let tick = ProfNote (mkAutoCC b mod) True True
      in (b, Tick tick e)
