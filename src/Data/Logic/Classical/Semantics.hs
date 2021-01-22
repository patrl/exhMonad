{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ParallelListComp #-}

module Data.Logic.Classical.Semantics where

import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( replicateM )
import           Data.Logic.Classical.Alternatives
                                                ( alts )
import           Data.Logic.Classical.Syntax
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
-- import qualified Data.Set                      as S
import           Data.Set                hiding ( foldr )
import qualified Text.Layout.Table             as T

import Data.Logic.Classical.Parser (parseExpr)

type Assignment = M.Map Var Bool


-- Interpret the logical operators of classical logic as haskell's boolean operators
interpretB :: BOp -> (Bool -> Bool -> Bool)
interpretB And = (&&)
interpretB Or  = (||)

-- returns a set of the distinct variables in an expression of classical logic.
variables :: CExpr -> Set Var
variables = foldr insert empty

universe :: Set Var -> Set Assignment
universe vs = fromList
  [ M.fromList [ (v, t) | v <- toList vs | t <- ts ]
  | ts <- replicateM (length vs) [True, False]
  ]

truthVals :: [Bool]
truthVals = [True, False]

-- takes an expression of classical logic, and returns a set of assignments
assignments :: CExpr -> Set Assignment
assignments = universe . variables

evaluate :: Assignment -> CExpr -> Maybe Bool
evaluate g (Simple t      ) = M.lookup t g -- evaluate a variable as a boolean
evaluate g (Unary Not expr) = not <$> evaluate g expr
evaluate g (Binary op expr1 expr2) =
  (liftA2 $ interpretB op) (evaluate g expr1) (evaluate g expr2)
evaluate g (Unary Exh expr) =
  let prejacent = evaluate g expr
  in  liftA2
        (&&)
        prejacent
        (foldr (liftA2 (&&))
               (Just True)
               [ not <$> evaluate g p | p <- altsExcl expr ]
        )

excludable :: CExpr -> CExpr -> Bool
excludable prej alt = not (prej `entails` alt)

altsExcl :: CExpr -> [CExpr]
altsExcl prej = concat [ [ alt | excludable prej alt ] | alt <- alts prej ]

-- return the truth-table for an expression of classical logic
truthTable :: CExpr -> String
truthTable expr =
  let vs        = variables expr
      rowLength = length vs + 1
  in  T.tableString
        (replicate rowLength T.def)
        T.unicodeRoundS
        T.def
        (   T.rowG
        <$> ((show <$> toList vs) ++ [show expr])
        : -- header row
            [ outputs g ++ [show . fromJust $ evaluate g expr]
            | g <- toList $ assignments expr
            ] -- values for each row
        )

outputs :: Assignment -> [String]
outputs g = [ show t | (_, t) <- M.toList g ]

testExpr :: Expr Var
testExpr = Binary Or (toExpr 'p') (Binary And (toExpr 'q') (toExpr 'r'))

isTautology :: CExpr -> Bool
isTautology expr = let gs = assignments expr in
  gs == verifiers expr gs

-- >>> isTautology <$> (parseExpr "p|(~p)")
-- Right True

-- >>> isTautology <$> (parseExpr "p&(~p)")
-- Right False

-- The following relies on the excluded middle
isContradiction :: CExpr -> Bool
isContradiction expr = verifiers expr (assignments expr) == empty


-- >>> isContradiction <$> (parseExpr "p&(~p)")
-- Right True

isContingent :: CExpr -> Bool
isContingent expr = not (isTautology expr || isContradiction expr)

-- >>> isContingent <$> (parseExpr "p|p")
-- Right True

isEquivalent :: CExpr -> CExpr -> Bool
isEquivalent expr1 expr2 =
  let vs = variables expr1 `union` variables expr2
      gs = universe vs
  in  verifiers expr1 gs == verifiers expr2 gs

entails :: CExpr -> CExpr -> Bool
expr1 `entails` expr2 =
  let vs = variables expr1 `union` variables expr2
      gs = universe vs
  in  verifiers expr1 gs `isSubsetOf` verifiers expr2 gs

verifiers :: CExpr -> Set Assignment -> Set Assignment
verifiers expr gs = fromList $ do
  g <- toList gs
  [ g | evaluate g expr == Just True ]

printTruthTable :: String -> IO ()
printTruthTable s = case parseExpr s of
  Left _ -> putStrLn "couldn't parse expression"
  (Right expr) -> putStrLn $ truthTable expr

-- >>> testExpr
-- (p ∨ (q ∧ r))

-- >>> assignments testExpr
-- fromList [fromList [(p,False),(q,False),(r,False)],fromList [(p,False),(q,False),(r,True)],fromList [(p,False),(q,True),(r,False)],fromList [(p,False),(q,True),(r,True)],fromList [(p,True),(q,False),(r,False)],fromList [(p,True),(q,False),(r,True)],fromList [(p,True),(q,True),(r,False)],fromList [(p,True),(q,True),(r,True)]]

-- >>> verifiers testExpr (assignments testExpr)
-- fromList [fromList [(p,False),(q,True),(r,True)],fromList [(p,True),(q,False),(r,False)],fromList [(p,True),(q,False),(r,True)],fromList [(p,True),(q,True),(r,False)],fromList [(p,True),(q,True),(r,True)]]
