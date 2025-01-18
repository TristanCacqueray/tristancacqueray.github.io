-- | The NoteFN function to dynamically add web components
module WebComponents (addWebComponents) where

import Emanote.Model.Note
import RIO

import Data.Aeson
import Data.List (nub)
import Data.Text qualified as Text
import Emanote.Model.SData (mergeAeson)
import Text.Pandoc.Definition qualified as P
import Text.Pandoc.Walk

import Data.Monoid (Any (..))
import Data.String.QQ (s)

webComponents :: [(Text, Text)]
webComponents =
    [ ("peaks-player", mkUrl "peaks-player")
    , ("peaks-playlist", mkUrl "peaks-player")
    , ("weiqi-visualizer", mkUrl "weiqi-visualizer")
    ]
  where
    mkUrl n = [s|<script src="https://cdn.midirus.com/script/|] <> n <> [s|.js"></script>|]

mermaidHtml :: Text
mermaidHtml =
    [s|
<script src="https://cdn.midirus.com/script/mermaid@11/mermaid.min.js" integrity="sha384-B2tp/GqmE6VfDRB3JPTsesr0+SXypThjLSvQEQH7iv3f3/PYKCm5Q4+SGPcitStz" crossorigin="anonymous"></script>
<script type="module">
  mermaid.initialize({ startOnLoad: false });
  mermaid.init(undefined,document.querySelectorAll(".mermaid"));
</script>
|]

rawHtml :: Text
rawHtml =
    [s|
    <style>
    code.raw {
        background-color: rgb(29, 31, 33);
        color: rgb(197, 200, 198);
        width: 100%;
        height: 100%;
        display: block;
        padding: 14px 18px;
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace;
    }
    </style>
|]
kbdHtml :: Text
kbdHtml =
    [s|
    <style>
    kbd {
        display: inline-block;
        padding: 0.25rem;
        font: 11px mono;
        line-height: 10px;
        color: #1f2328;
        vertical-align: middle;
        background-color: rgb(246, 248, 250);
        border: solid 1px #1f232826;
        border-bottom-color: rgba(209, 217, 224, 0.7);
        border-radius: 6px;
        box-shadow: inset 0 -1px 0 #d1d9e0b3;
      }
    </style>
|]
badukHtml :: Text
badukHtml =
    [s|
    <script src="/static/godiag.js"></script>
    <style>
    code.baduk {
      background-color: white;
      color: black;
    }
    </style>
|]
mathHtml :: Text
mathHtml =
    [s|
    <script>
    window.MathJax = {
      startup: {
        ready: () => {MathJax.startup.defaultReady();}
      }
    };
    </script>
    <script type="text/javascript" id="MathJax-script" async src="https://cdn.midirus.com/script/mathjax@3/tex-mml-chtml.js" integrity="sha384-Wuix6BuhrWbjDBs24bXrjf4ZQ5aFeFWBuKkFekO2t8xFU0iNaLQfp2K6/1Nxveei" crossorigin="anonymous"></script>
|]

preHighlighHtml, postHighlighHtml :: Text
preHighlighHtml =
    [s|
    <link rel="stylesheet" href="https://cdn.midirus.com/script/highlightjs@11/hybrid.min.css" integrity="sha384-hQMbcFx9Gwko5P0sCJ32kiMnKKb4rDh8mMsSkp3p5GRxiS33HQyalu+R0Xxa0Raa" crossorigin="anonymous">
    <script src="https://cdn.midirus.com/script/highlightjs@11/highlight.min.js" integrity="sha384-F/bZzf7p3Joyp5psL90p/p89AZJsndkSoGwRpXcZhleCWhd8SnRuoYo4d0yirjJp" crossorigin="anonymous"></script>
|]
postHighlighHtml =
    [s|
    <script>
    hljs.registerAliases(["lisp", "elisp"],   {languageName: "scheme"})
    hljs.registerAliases("reascript", {languageName: "javascript"})
    hljs.registerAliases("strudel", {languageName: "javascript"})
    hljs.highlightAll();
    </script>
|]

genDynamicBlock :: P.Block -> [Text]
genDynamicBlock = \case
    P.CodeBlock (_, classes, _) _
        | "mermaid" `elem` classes -> [mermaidHtml]
        | "raw" `elem` classes -> [rawHtml]
        | "baduk" `elem` classes -> [badukHtml]
        | "haskell" `elem` classes -> [[s|<script src="https://cdn.midirus.com/script/highlightjs@11/haskell.min.js" integrity="sha384-EnLqy/LmA2yShypXJIP5cvUz0S3jJ5l3PnZcMRE6JQdpeNOttU97yyQAsdTusDbl" crossorigin="anonymous"></script>|]]
        | "nix" `elem` classes -> [[s|<script src="https://cdn.midirus.com/script/highlightjs@11/nix.min.js" integrity="sha384-Cu93ZX4MeViuo8zGfs3i/Rbyj81UUFg0DLBKhn63FlLi/qoH39kOO++xzdoUJq/Y" crossorigin="anonymous"></script>|]]
        | "scheme" `elem` classes || "elisp" `elem` classes ->
            [[s|<script src="https://cdn.midirus.com/script/highlightjs@11/scheme.min.js" integrity="sha384-wDAOgnGx0t7X9tTKddfqp7kcfAYVDaQDRdfDD+47IukNBVuVGFldV80H4qDTHPOR" crossorigin="anonymous"></script>|]]
    _ -> []

genDynamicInline :: P.Inline -> [Text]
genDynamicInline = \case
    P.RawInline (P.Format "html") tag
        | tag == "<kbd>" -> [kbdHtml]
        | Just webComponent <- lookup (Text.drop 2 $ Text.dropEnd 1 $ tag) webComponents -> [webComponent]
    P.Math _ _ -> [mathHtml]
    _ -> []

hasCode :: P.Block -> Any
hasCode = \case
    P.CodeBlock (_, classes, _) _
        | classes /= [] && classes /= ["raw"] && "query" `notElem` classes -> Any True
    _ -> Any False

addHighlightJS :: P.Pandoc -> Text -> Text
addHighlightJS blk txt = case query hasCode blk of
    Any True -> preHighlighHtml <> txt <> postHighlighHtml
    Any False -> txt

addWebComponents :: Note -> Note
addWebComponents note =
    {-
      ( if (_noteTitle note == "Pastagang")
          then (trace (Text.pack (show (_noteDoc note)) <> "\n" <> _dynhead))
          else id
      )
          $ -} note{_noteMeta = mergeAeson (_noteMeta note) newMeta}
  where
    doc = _noteDoc note
    dynHead = query genDynamicBlock doc <> query genDynamicInline doc
    _dynhead = addHighlightJS doc $ Text.unlines $ nub dynHead
    newMeta = object ["_dynhead" .= _dynhead]
