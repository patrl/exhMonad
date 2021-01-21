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

-- The following only work due to the excluded middle
isTautology :: CExpr -> Bool
isTautology expr = all (\t -> t == Just True)
                       ([ evaluate g expr | g <- toList $ assignments expr ])

isContradiction :: CExpr -> Bool
isContradiction expr = all
  (\t -> t == Just False)
  ([ evaluate g expr | g <- toList $ assignments expr ])

isContingent :: CExpr -> Bool
isContingent expr = and
  [ t `elem` [ evaluate g expr | g <- toList $ assignments expr ]
  | t <- [Just True, Just False]
  ]

isEquivalent :: CExpr -> CExpr -> Bool
isEquivalent expr1 expr2 =
  let vs = variables expr1 `union` variables expr2
      gs = universe vs
  in  [ evaluate g expr1 | g <- toList gs ]
      == [ evaluate g expr2 | g <- toList gs ]

-- The classical notion of logical consequence; checks that the assignments that verify expr1 are a subset
-- of the assignments that verify expr2.
entails :: CExpr -> CExpr -> Bool
expr1 `entails` expr2 =
  let
    vs = variables expr1 `union` variables expr2
    gs = universe vs
  in
    (fromList . concat)
        [ [ g | evaluate g expr1 == Just True ] | g <- toList gs ]
      `isSubsetOf` (fromList . concat)
                     [ [ g | evaluate g expr2 == Just True ] | g <- toList gs ]
