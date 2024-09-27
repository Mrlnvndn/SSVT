{-# LANGUAGE OverloadedStrings #-}
module Exercise6 where

import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as A
import Control.Monad
import Text.Blaze.Html4.FrameSet (docTypeHtml)
import qualified Text.Blaze.Html.Renderer.Pretty as HTML

{--
Exercise 6: Visualize results

We already tried to show some of the interim results and pretty-print results in the other exercises.
Here we wanted to play around a bit with other ways of showing these figures that are possible in haskell.

Some options we considered:
- Using QuickPlot, but we figured our limited amount of results did not warrant anything complex

In this module we combine all the functions and reporting we did before with some extra metrics. We decided to show this
both in the terminal and in HTML format (inspired by the Stryker presentation).

Haskell has a very nice HTML rendering library called blaze-html that turned out quite easy to use.

--}


simpleTemplate :: (HTML.ToMarkup a) => [[a]] -> HTML.Html
simpleTemplate resTable = docTypeHtml $ do
    HTML.head $ do
        HTML.title $ "Mutation Testing Results"
    HTML.body $ do
        HTML.p $ "Results for function: --"
        tableTemplate resTable


tableTemplate :: (HTML.ToMarkup a) => [[a]] -> HTML.Html
tableTemplate xs = HTML.table $ forM_ xs (HTML.tr . mapM_ (HTML.td . HTML.toHtml))


main = do
    let filename = "mutation_test_results.html"
    let table :: [[String]] = [
            ["Property Name", "Metric", "Result", "Time taken"],
            ["Property_1", "anything", "1", "0 sec"]]
    let html = simpleTemplate table
    writeFile filename $ HTML.renderHtml html