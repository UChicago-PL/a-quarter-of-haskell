{-# LANGUAGE OverloadedStrings #-}

module QuineHtml where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map,head,main,var)
import Text.Blaze.Html5.Attributes hiding (id,min,title,item,pattern)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.String (renderHtml)

import Proposition
import Quine

type ProofObject = (Int,Either String (Proposition, Analysis))

newtype WrappedProposition = WrappedProposition Proposition

instance ToMarkup WrappedProposition where
    toMarkup (WrappedProposition prop) = "$" *> toTeX prop *> "$"

toHtml' :: Proposition -> Html
toHtml' = toHtml . WrappedProposition

toTeX :: Proposition -> Html
toTeX = prec 0 where
    prec _ (Boolean bool) = boolDict Map.! bool
    prec _ (Var v) = greekDict Map.! v
    prec _ (Not prop) = "{\\lnot}" >> prec 3 prop
    prec ix(And s t) = paren 2 ix $
        prec 2 s >> "{\\,\\land\\,}" >> prec 2 t
    prec ix (Or s t) = paren 1 ix $
        prec 1 s >> "{\\,\\lor\\,}" >> prec 1 t
    prec ix (Implies s t) = paren 0 ix $
        prec 1 s >> "{\\;\\limplies\\;}" >> prec 0 t
    paren :: Int -> Int -> Html -> Html
    paren cutoff prec' markup
        | prec' > cutoff = "(" >> markup >> ")"
        | otherwise = markup

greekDict :: Map String Html
greekDict = Map.fromList $
    [ ("a","{\\alpha}"),("b","{\\beta}"),("c","{\\gamma}")
    , ("d","{\\delta}"),("e","{\\epsilon}"),("f","{\\zeta}")
    , ("g","{\\eta}"),("h","{\\theta}"), ("i","{\\iota}")
    , ("j","{\\kappa}"),("k","{\\lambda}"),("l","{\\mu}")
    , ("m","{\\nu}"),("n","{\\xi}"),("o","{\\omicron}")
    , ("p","{\\pi}"),("q","{\\rho}"),("r","{\\sigma}"),("s","{\\tau}")
    , ("t","{\\upsilon}"),("u","{\\phi}"),("v","{\\chi}")
    , ("w","{\\psi}"),("x","{\\omega}")]

boolDict :: Map Bool Html
boolDict = Map.fromList $
    [ (True,"{\\ltrue}")
    , (False,"{\\lfalse}")
    ]

renderItem :: ProofObject -> Html
renderItem (ix,item) = case item of
    Right (prop,analysis) -> renderAnalysis ix prop analysis
    Left str -> renderUnparseable ix str

renderUnparseable :: Int -> String -> Html
renderUnparseable ix str = do
    h2 $ "Unparseable " >> toHtml ix >> ": " >> toHtml (show str)
    p ! class_ "box" $
        "$\\Diamond$ Unparseable " >> toHtml ix

renderAnalysis :: Int -> Proposition -> Analysis -> Html
renderAnalysis ix prop analysis = case analysis of
    Right proof -> renderProof ix prop proof
    Left refutation -> renderRefutation ix prop refutation

renderProof :: Int -> Proposition -> QuineProof -> Html
renderProof ix prop proof = do
    h2 $ "Proposition " >> toHtml ix >> ": " >> toHtml' prop
    format proof
    p ! class_ "box" $
        "$\\Box$ Proposition " >> toHtml ix
    where
    format prf = case prf of
        Split props var trueb falseb -> do
            when (length props > 1) $ do
                p "Simplification"
                ul . void $ traverse (li . toHtml') props
            let prop' = last props
            p "Quine Alternatives:"
            ul $ do
                li $ showBranch prop' var True trueb
                li $ showBranch prop' var False falseb
        Trivial props -> do
            p "Simplification"
            ul . void $ traverse (li . toHtml') props
            p $ "Reduced to " >> toHtml' (Boolean True) >> "."
        Reference props citation pattern bindings -> do
            ul . void . traverse (li . toHtml') $ props
            p $ "Substitution instance of Proposition "
                >> toHtml citation
                >> " $[" >> toTeX pattern >> "]$ at"
            ul . void . flip Map.traverseWithKey bindings
               $ \k v -> li $ "$" >> greekDict Map.! k >> " := "
                                  >> toTeX v >> "$"
    showBranch prop' var bool branch = p $ do
        toHtml' prop' >> " $[" >> greekDict Map.! var >> ":="
            >> boolDict Map.! bool >> "]$"
        format branch

renderRefutation :: Int -> Proposition -> Refutation -> Html
renderRefutation ix prop (Refutation refutation) = do
    h2 $ "Refutable " >> toHtml ix >> ": " >> toHtml' prop
    p "Refuting valuation"
    ul . void . flip Map.traverseWithKey refutation $ \k v ->
        li $ "$" >> greekDict Map.! k >> " := "
                >> boolDict Map.! v >> "$"
    p $ "Reduced to " >> toHtml' (Boolean False) >> "."
    p ! class_ "box" $
            "$\\Diamond$ Refutable" >> toHtml ix

renderQuine :: [ProofObject] -> String
renderQuine objects = renderHtml. docTypeHtml $ do
    H.head $ do
        title "Propositional Tautologies"
        script ! type_ "text/javascript"
               ! src mathjax
               $ ""
        script ! type_ "text/x-mathjax-config"
               $ config
        H.style ! type_ "text/css"
                $ css
    body $ do
        h1 "Propositional Tautologies"
        void $ traverse renderItem objects
    where
    mathjax = stringValue $
      "https:"
      ++ "//cdnjs.cloudflare.com/"
      ++ "ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML"
    config = do
        "MathJax.Hub.Config({"
        "  tex2jax: {"
        "    inlineMath:  [['$','$']]"
        "  },"
        "  TeX: {"
        "    Macros: {"
        "      limplies: \"{\\\\rightarrow}\","
        "      ltrue: \"{\\\\top}\","
        "      lfalse: \"{\\\\bot}\","
        "      proves: \"{\\\\vdash}\"}"
        "    } "
        "});"

    css = "h1,h2,p.box { color: maroon }"
