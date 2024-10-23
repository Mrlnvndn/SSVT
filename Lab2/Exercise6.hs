{-# LANGUAGE OverloadedStrings #-}

module Lab2.Exercise6 where

import Control.Monad
-- Functions to visualize

import Lab2.Exercise2 (countSurvivors)
import Lab2.Exercise3 (minPropSubset)
import Lab2.FitSpec (multiplicationTable, propMap, properties')
import Lab2.Mutation (mutators)
import Lab2.Utils (MutationMap, PropMap, TypeFut, mapKeys, printResMap)
import Text.Blaze.Html.Renderer.Pretty qualified as HTML
import Text.Blaze.Html4.FrameSet (docTypeHtml)
import Text.Blaze.Html5 qualified as HTML
import Text.Blaze.Html5.Attributes qualified as A

-- Time Spent: 230 min

{--
Exercise 6: Visualize results

-- Note: install library 'blaze-html' before running

We already tried to show some of the interim results and pretty-print results in the other exercises.
Here we wanted to play around a bit with other ways of showing these figures that are possible in haskell.

Options we considered:
- Pretty printing some metrics to the terminal
- Outputting an HTML report (similar to Stryker)
- Using QuickPlot, but we figured our limited amount of results did not warrant anything complex

In this module we combine most of the functions and reporting we did before with some extra metrics.
We decided to show this both in the terminal and in HTML format (inspired by the Stryker presentation).

Haskell has a very reasonable HTML rendering library called blaze-html that turned out quite easy to use.
At least for a simple proof of concept like this.
We partially used implementation from: https://gdevanla.github.io/posts/read-you-a-blaze.html
After running the main function you will overwrite mutation_test_results.html
(repo already contains one for a low (100) and high amount of mutants to highlight the difference in mps)

## What can be visualized?

- Amount of mutators used as well as mutations
- Properties with their names

- amount of survivors -> countSurvivors
- Minimal Property Subset
- Strength for each property (could also add average strength or highest and lowest)
- Equivalencies in properties

Generally it was quite hard to rewrite all functions to neatly fit in these visualisations. Especially the equivalency
and strengths we deemed a bit too time consuming. They are already quite nicely visualised in their respective
modules.

We opted to both do a report in the terminal as by HTML report. To make the result a bit nicer we used
a public stylesheet from simple.css and a great image :)

Of course all calculations are now done multiple times so this is very inefficient, but for this demonstration
we did not care to write more optimized / streamlined code.

--}

reportTitle :: HTML.Html
reportTitle = "Mutation Testing Results"

mutationTestingTemplate :: (HTML.ToMarkup a) => [[a]] -> String -> [a] -> [a] -> HTML.Html
mutationTestingTemplate resTable functionName allProps minProps = docTypeHtml $ do
  HTML.head $ do
    HTML.title $ reportTitle
    HTML.link HTML.! A.rel "stylesheet" HTML.! A.type_ "text/css" HTML.! A.href "https://cdn.simplecss.org/simple.min.css"
  HTML.body $ do
    HTML.img HTML.! A.src "https://i.redd.it/gtw5oqyllmf91.jpg" HTML.! A.style "height: 200px; width: 100%"
    HTML.h1 "Mutation Testing Results"
    HTML.p $ HTML.toHtml ("Results for function: " ++ (show functionName))
    tableTemplate resTable
    HTML.hr
    HTML.h3 "Properties"
    HTML.p "From all properties (given amount of mutants):"
    listTemplate allProps
    HTML.p "We get the following properties for a minimal subset:"
    listTemplate minProps

listTemplate :: (HTML.ToMarkup a) => [a] -> HTML.Html
listTemplate xs = HTML.ul $ forM_ xs (HTML.li . HTML.toHtml)

tableTemplate :: (HTML.ToMarkup a) => [[a]] -> HTML.Html
tableTemplate xs = HTML.table $ forM_ xs (HTML.tr . mapM_ (HTML.td . HTML.toHtml))

sep = "\n*-----------------------*\n"

terminalReport :: PropMap -> Integer -> Integer -> Integer -> MutationMap -> IO ()
terminalReport props nMutators nMutations survivors mps = do
  putStrLn $ sep ++ "REPORT OF MUTATION TESTING RESULTS:" ++ sep ++ "\n"

  putStrLn $
    "Used "
      ++ show nMutators
      ++ " mutants to create "
      ++ show nMutations
      ++ " mutations\n"
      ++ "There were "
      ++ show survivors
      ++ " surviving mutations"
      ++ sep

  putStrLn $
    "After eliminating properties that: don't kill any mutants,\n"
      ++ "equivalent properties and properties that are subsets of another prop\n"
      ++ "we get the Minimal Property Subset:"

  putStrLn $ show $ mapKeys mps

visualizeMutationTest :: TypeFut -> Integer -> String -> IO ()
visualizeMutationTest fut nMutants functionName = do
  -- Calculate some results
  let props = propMap
  let nMutators = fromIntegral (length mutators)
  survivors <- countSurvivors nMutants properties' fut
  mps <- minPropSubset fut props nMutants

  -- Print results to the terminal
  terminalReport props nMutators nMutants (fromIntegral survivors) mps

  -- Create html file using Blaze
  let filename = "mutation_test_results.html"
  let table :: [[String]] =
        [ ["Property Name", "Result"],
          ["Mutators", show nMutators],
          ["Mutations", show nMutants],
          ["Survived mutants", show survivors],
          ["Killed mutants", show (nMutants - fromIntegral survivors)],
          ["Props in minimal property subset", show (length $ mapKeys mps)]
        ]
  let html = mutationTestingTemplate table functionName
  writeFile filename $ HTML.renderHtml $ html (mapKeys propMap) (mapKeys mps)

main :: IO ()
main = visualizeMutationTest multiplicationTable 100000 "multiplicationTable"
