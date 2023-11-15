-- | Very simple visualisation of BDDs using /dot/.

module Data.HasCacBDD.Visuals (
  genGraph,
  genGraphWith,
  showGraph,
  svgGraph
) where

import Data.Maybe (fromJust)
import System.Exit
import System.IO
import System.Process

import Data.HasCacBDD

-- | Generate a string which describes the BDD in the dot language.
genGraph :: Bdd -> String
genGraph = genGraphWith show

-- | Given a function to show variables, generate a string which describes the BDD in the dot language.
genGraphWith :: (Int -> String) -> Bdd -> String
genGraphWith myShow myb
  | myb == bot = "digraph g { Bot [label=\"0\",shape=\"box\"]; }"
  | myb == top = "digraph g { Top [label=\"1\",shape=\"box\"]; }"
  | otherwise = "strict digraph g {\n" ++ links ++ sinks ++ rankings ++ "}" where
      (links,topdone) = genGraphStep [] myb
      genGraphStep :: [(Bdd,Int)] -> Bdd -> (String,[(Bdd,Int)])
      genGraphStep done curB =
        if curB `elem` [top,bot] ++ map fst done then ("",done) else
            let
              thisn = if null done then 0 else maximum (map snd done) + 1
              thisnstr = show thisn
              thisvar = fromJust $ firstVarOf curB
              out1  = "n" ++ thisnstr ++ " [label=\"" ++ myShow thisvar ++ "\",shape=\"circle\"];\n"
              (lhs, rhs) = (thenOf curB, elseOf curB)
              (lhsoutput,lhsdone) = genGraphStep ((curB,thisn):done) lhs
              leftn = fromJust $ lookup lhs lhsdone
              out2
                | lhs == top = "n"++ thisnstr ++" -> Top;\n"
                | lhs == bot = "n"++ thisnstr ++" -> Bot;\n"
                | otherwise  = "n"++ thisnstr ++" -> n" ++ show leftn ++";\n" ++ lhsoutput
              (rhsoutput,rhsdone) = genGraphStep lhsdone rhs
              rightn = fromJust $ lookup rhs rhsdone
              out3
                | rhs == top = "n"++ thisnstr ++" -> Top [style=dashed];\n"
                | rhs == bot = "n"++ thisnstr ++" -> Bot [style=dashed];\n"
                | otherwise  = "n"++ thisnstr ++" -> n"++ show rightn ++" [style=dashed];\n" ++ rhsoutput
            in (out1 ++ out2 ++ out3, rhsdone)
      sinks = "Bot [label=\"0\",shape=\"box\"];\n" ++ "Top [label=\"1\",shape=\"box\"];\n"
      rankings = concat [ "{ rank=same; "++ unwords (nodesOf v) ++ " }\n" | v <- allVarsOf myb ]
      nodesOf v = map (("n"++).show.snd) $ filter ( \(b,_) -> firstVarOf b == Just v ) topdone

-- | Display the graph of a BDD with dot.
showGraph :: Bdd -> IO ()
showGraph b = do
  (inp,_,_,pid) <- runInteractiveProcess "/usr/bin/dot" ["-Tx11"] Nothing Nothing
  hPutStr inp (genGraph b)
  hFlush inp
  hClose inp
  _ <- waitForProcess pid
  return ()

-- | Generate SVG of a BDD with dot.
svgGraph :: Bdd -> IO String
svgGraph b = do
  (exitCode,out,err) <- readProcessWithExitCode "/usr/bin/dot" ["-Tsvg" ] (genGraph b)
  case exitCode of
    ExitSuccess -> return $ (unlines.tail.lines) out
    ExitFailure n -> error $ "dot -Tsvg failed with exit code " ++ show n ++ " and error: " ++ err
