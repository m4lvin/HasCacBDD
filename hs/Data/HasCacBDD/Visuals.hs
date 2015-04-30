-- | Very simple visualisation of BDDs using /dot/.

module Data.HasCacBDD.Visuals (
  genGraph,
  showGraph
) where

import Data.HasCacBDD
import Data.List (nub,sort)
import System.Process
import System.IO

type Note = [Int]
data AnnotatedBdd = ATop Note | ABot Note | AVar Int AnnotatedBdd AnnotatedBdd Note deriving (Show,Eq)

varsOf :: BddTree -> [Int]
varsOf Top = []
varsOf Bot = []
varsOf (Var n lhs rhs) = sort $ nub (n: varsOf lhs ++ varsOf rhs)

noteOf :: AnnotatedBdd -> Note
noteOf (ABot n) = n
noteOf (ATop n) = n
noteOf (AVar _ _ _ n) = n

annotate :: BddTree -> AnnotatedBdd
annotate Bot = ABot [0]
annotate Top = ATop [1]
annotate (Var k lhs rhs) = AVar k (annotate lhs) (annotate rhs) $
  if noteOf (annotate lhs) == noteOf (annotate rhs)
    then noteOf (annotate lhs)
    else (k:noteOf (annotate lhs)) ++ (k:noteOf (annotate rhs))

allLabels :: AnnotatedBdd -> [Note]
allLabels ab = nub $ allLabels' ab where
  allLabels' (ABot n) = [n]
  allLabels' (ATop n) = [n]
  allLabels' (AVar _ lhs rhs l) = [l] ++ allLabels lhs ++ allLabels rhs

-- | Generate a string which describes the BDD in the dor language.
genGraph :: Bdd -> String
genGraph myb = genGraph' (unravel myb) where
  genGraph' (Bot) = "digraph g { Bot [label=\"0\",shape=\"box\"]; }"
  genGraph' (Top) = "digraph g { Top [label=\"1\",shape=\"box\"]; }"
  genGraph' b = "strict digraph g {\n" ++ genGraphStep (annotate b) ++ sinks ++ rankings ++ "}"
    where
      genGraphStep (AVar v lhs rhs l) =
	"n" ++ lp l ++ " [label=\"" ++ show v ++ "\",shape=\"circle\"];\n"
	++ case lhs of
	  (ATop _) -> "n"++ lp l ++" -> Top;\n"
	  (ABot _) -> "n"++ lp l ++" -> Bot;\n"
	  (AVar _ _ _ l') -> "n"++ lp l ++" -> n"++ lp l' ++";\n" ++ genGraphStep lhs
	++ case rhs of
	  (ATop _) -> "n"++ lp l ++" -> Top [style=dashed];\n"
	  (ABot _) -> "n"++ lp l ++" -> Bot [style=dashed];\n"
	  (AVar _ _ _ l') -> "n"++ lp l ++" -> n"++ lp l' ++" [style=dashed];\n" ++ genGraphStep rhs
      genGraphStep _ = ""
      sinks = "Bot [label=\"0\",shape=\"box\"];\n" ++ "Top [label=\"1\",shape=\"box\"];\n"
      rankings = concat [ "{ rank=same; "++ unwords (nub $ nodesOf v (annotate b)) ++ " }\n" | v <- varsOf b ]
      nodesOf _ (ABot _) = []
      nodesOf _ (ATop _) = []
      nodesOf v (AVar v' lhs rhs l) = if v==v' then ["n"++lp l] else nodesOf v lhs ++ nodesOf v rhs
      lp l = show n where (Just n) = lookup l nodelabelling
      nodelabelling = zip (allLabels (annotate b)) [(0::Int)..]

-- | Display the graph of a BDD with dot.
showGraph :: Bdd -> IO ()
showGraph b = do
  (inp,_,_,pid) <- runInteractiveProcess "/usr/bin/dot" ["-Tx11"] Nothing Nothing
  hPutStr inp (genGraph b)
  hFlush inp
  hClose inp
  _ <- waitForProcess pid
  return ()
