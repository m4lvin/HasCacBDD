-- | Very simple visualisation of BDDs using /dot/.

module Data.HasCacBDD.Visuals (
  genGraph,
  genGraphWith,
  showGraph,
  svgGraph
) where

import Data.HasCacBDD
import System.Process
import System.IO

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
      genGraphStep done curB = case (lookup curB done, curB == top || curB == bot) of
        (_     ,True) -> ("",done)
        (Just _, _) -> ("",done)
        (Nothing,False) ->
            let
              thisn = if null done then 0 else maximum (map snd done) + 1
              thisnstr = show thisn
              (Just thisvar) = firstVarOf curB
              out1  = "n" ++ thisnstr ++ " [label=\"" ++ myShow thisvar ++ "\",shape=\"circle\"];\n"
              lhs   = thenOf curB
              (lhsoutput,lhsdone) = genGraphStep ((curB,thisn):done) lhs
              (Just leftn) = lookup lhs lhsdone
              out2
                | lhs == top = "n"++ thisnstr ++" -> Top;\n"
                | lhs == bot = "n"++ thisnstr ++" -> Bot;\n"
                | otherwise  = "n"++ thisnstr ++" -> n" ++ show leftn ++";\n" ++ lhsoutput
              rhs   = elseOf curB
              (rhsoutput,rhsdone) = genGraphStep lhsdone rhs
              (Just rightn) = lookup rhs rhsdone
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
  (inp,out,_,pid) <- runInteractiveProcess "/usr/bin/dot" ["-Tsvg" ] Nothing Nothing
  hPutStr inp (genGraph b)
  hSetBinaryMode inp False
  hSetBinaryMode out False
  hFlush inp
  hClose inp
  outstring <- hGetContents out
  _ <- waitForProcess pid
  return $ (unlines.tail.lines) outstring
