module Exercise6 where

import Data.Graph.DGraph
import Data.Graph.Visualize
import Data.Graph.Types
import LTS
import Exercise2 (ltsGen)
import Test.QuickCheck (generate)
import Control.Monad (void)


-- ## Indication of time spent: 50 Minutes ##

{-
Exercise 6: Visualize IOLTS

We had a bit of time (distracting ourselves from the exam) to create a quick visualisation.
The idea was just to plot all states and transtitions using a directed graph. Googling this
we got the Data.Graph library, which is from a lib called 'graphite'.


Somehow outputting a png did not print labels correctly,
 so instead we resorted to using plotDGraphEdged, which creates a popup.
If you run main (or visualiseRandom) you will get a PopUp image with a rendering of the door model
  below, as well as a randomly generated IOLTS model.

This works quite well, but could of course be made more clear and prettier.
However, we thought this was enough of a distraction :)

-}

-- We use a model where there is no overlap in arrows (like we used for ex5)
-- This renders better and is the better way to define an IOLTS
doorModel :: IOLTS
doorModel = createIOLTS [
                (0,"?close", 1),
                (1,"!closed",2),
                (2,"?open", 3),
                (3,"!opened",0),
                (2, "?lock", 4),
                (4,"!locked",5),
                (5, "?unlock", 6),
                (6,"!unlocked",2)]

-- visualisation function
visualizeLTS :: IOLTS -> IO ()
visualizeLTS lts@(_, _, _, transitions, _) =
    let arcs = map (\(q, label, q_next) -> Arc q q_next label) transitions
        graph = fromArcsList arcs
      in void $ plotDGraphEdged graph


-- It also works for random IOLTS models 
-- The labels consist of random strings
-- Function first generates an IOLTS, shows it and then pops up a visualisation window
visualizeRandom :: IO ()
visualizeRandom = do
    randomIOLTS <- generate ltsGen
    putStrLn $ "Randomly generated IOLTS: " ++ show randomIOLTS
    visualizeLTS randomIOLTS


-- main function which calls it with our generator.
main = do
    putStrLn $ "### Visualise a correct door model: ###\n" ++ show doorModel
    visualizeLTS doorModel

    putStrLn "### Visualise random IOLTS ###"
    visualizeRandom
