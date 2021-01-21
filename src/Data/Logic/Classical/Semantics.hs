module Data.Logic.Classical.Semantics where

import           Control.Monad                  ( foldM
                                                , liftM2
                                                , replicateM
                                                )
import           Data.Logic.Classical.Syntax
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
import qualified Data.Set                      as S
import qualified Text.Layout.Table             as T

type Assignment = M.Map Var Bool

interpretU :: UOp -> (Bool -> Bool)
interpretU Not = not

interpretB :: BOp -> (Bool -> Bool -> Bool)
interpretB And = (&&)
interpretB Or  = (||)

-- returns a set of the distinct variables in an expression of classical logic.
variables :: CExpr -> S.Set Var
variables = foldr S.insert S.empty

-- Given a set of variables, returns the set of possible assignments
universe :: S.Set Var -> S.Set Assignment
universe vs = S.fromList $ M.fromList . zip (S.toList vs) <$> replicateM
  (length vs)
  [True, False]

-- takes an expression of classical logic, and returns a set of assignments
assignments :: CExpr -> S.Set Assignment
assignments = universe . variables

--- Evaluate an expression of classical logic relative to an assignment by:
--- (i) replacing variables with boolean values
--- (ii) evaluating the resulting boolean expression
evaluate :: Assignment -> CExpr -> Maybe Bool
evaluate g expr = evaluateB <$> toBoolExpr g expr

-- Traverses an expression of classical logic, and replaces variables with boolean values.
toBoolExpr :: Assignment -> CExpr -> Maybe BExpr
toBoolExpr g = traverse (`M.lookup` g)

-- Evaluate a boolean expression
evaluateB :: BExpr -> Bool
evaluateB (Simple t     ) = t
evaluateB (Unary op expr) = interpretU op $ evaluateB expr
evaluateB (Binary op expr1 expr2) =
  interpretB op (evaluateB expr1) (evaluateB expr2)

-- return the truth-table for an expression of classical logic
truthTable :: CExpr -> String
truthTable expr =
  let vs        = S.toList $ variables expr
      rowLength = length vs + 1
  in  T.tableString
        (replicate rowLength T.def)
        T.unicodeRoundS
        T.def
        (   T.rowG
        <$> ((show <$> vs) ++ [show expr])
        : -- header row
            [ outputs g ++ [show . fromJust $ evaluate g expr]
            | g <- S.toList $ assignments expr
            ] -- values for each row
        )

outputs :: Assignment -> [String]
outputs g = [ show t | (_, t) <- M.toList g ]

-- The following only work due to the excluded middle
isTautology :: CExpr -> Bool
isTautology expr =
  Just False `notElem` ([ evaluate g expr | g <- S.toList $ assignments expr ])

isContradiction :: CExpr -> Bool
isContradiction expr =
  Just True `notElem` ([ evaluate g expr | g <- S.toList $ assignments expr ])

isContingent :: CExpr -> Bool
isContingent expr = and
  [ t `elem` [ evaluate g expr | g <- S.toList $ assignments expr ]
  | t <- [Just True, Just False]
  ]


-- -- $> putStrLn $ truthTable testExpr

testExpr :: CExpr
testExpr = Binary Or (toExpr 'p') (Binary Or (toExpr 'q') (toExpr 'r'))

-- $> testExpr

-- $> isTautology testExpr

-- $> isContingent testExpr

tautTest :: CExpr
tautTest = Binary Or (toExpr 'p') (Unary Not (toExpr 'p'))

-- $> tautTest

-- $> isTautology tautTest

-- $> isContingent testExpr

-- TODO write function testing semantic equivalence
