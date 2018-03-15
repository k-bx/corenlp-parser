{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module provides a handy wrapper around the CoreNLP project\'s
-- command-line utility https://nlp.stanford.edu/software/corenlp.html
-- , and a parser for some of its output formats.
module NLP.CoreNLP
  ( launchCoreNLP
  , parseJsonDoc
  , Dependency(..)
  , Entitymention(..)
  , Token(..)
  , Sentence(..)
  , PennPOS(..)
  , Coref(..)
  , CorefsId
  , Corefs
  , Document(..)
  , NamedEntity(..)
  -- * Internal
  , test
  ) where

import Control.Applicative
import Control.Monad (forM, when)
import qualified Data.Aeson as J
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import Data.Semigroup
import Data.String.Class as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import System.Exit
import System.IO (hFlush)
import System.IO.Temp
import System.Process
import Text.RawString.QQ (r)

-- | Contains generic "hacks" we put on top to go around limitations like a
-- "type" keyword we can't use
jsonOpts :: J.Options
jsonOpts = J.defaultOptions {J.fieldLabelModifier = fm}
  where
    fm :: String -> String
    fm "type_" = "type"
    fm x = x

data Dependency = Dependency
  { dep :: Text
  , governor :: Int
  , governorGloss :: Text
  , dependent :: Int
  , dependentGloss :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Dependency where
  parseJSON = J.genericParseJSON jsonOpts

instance ToJSON Dependency where
  toJSON = J.genericToJSON jsonOpts

data Entitymention = Entitymention
  { docTokenBegin :: Int
  , docTokenEnd :: Int
  , tokenBegin :: Int
  , tokenEnd :: Int
  , text :: Text
  , characterOffsetBegin :: Int
  , characterOffsetEnd :: Int
  , ner :: Text
  , normalizedNER :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON Entitymention where
  parseJSON = J.genericParseJSON jsonOpts

instance ToJSON Entitymention where
  toJSON = J.genericToJSON jsonOpts

data PennPOS
  = CC -- ^ Coordinating conjunction
  | CD -- ^ Cardinal number
  | DT -- ^ Determiner
  | EX -- ^ Existential *there*
  | FW -- ^ Foreign word
  | IN -- ^ Preposition or subordinating conjunction
  | JJ -- ^ Adjective
  | JJR -- ^ Adjective, comparative
  | JJS -- ^ Adjective, superlative
  | LS -- ^ List item marker
  | MD -- ^ Modal
  | NN -- ^ Noun, singular or mass
  | NNS -- ^ Noun, plural
  | NNP -- ^ Proper noun, singular
  | NNPS -- ^ Proper noun, plural
  | PDT -- ^ Predeterminer
  | POS -- ^ Possessive ending
  | PRP -- ^ Personal pronoun
  | PRPDollar -- ^ Possessive pronoun
  | RB -- ^ Adverb
  | RBR -- ^ Adverb, comparative
  | RBS -- ^ Adverb, superlative
  | RP -- ^ Particle
  | SYM -- ^ Symbol
  | TO -- ^ *to*
  | UH -- ^ Interjection
  | VB -- ^ Verb, base form
  | VBD -- ^ Verb, past tense
  | VBG -- ^ Verb, gerund or present participle
  | VBN -- ^ Verb, past participle
  | VBP -- ^ Verb, non-3rd person singular present
  | VBZ -- ^ Verb, 3rd person singular present
  | WDT -- ^ Wh-determiner
  | WP -- ^ Wh-pronoun
  | WPDollar -- ^ Possessive wh-pronoun
  | WRB -- ^ Wh-adverb
  | LRB -- ^ "-LRB-"? No idea what's this
  | RRB -- ^ "-RRB-"? No idea what's this
  | PosPunctuation Text -- ^ anyOf ".:,''$#$,", sometimes few together
  deriving (Show, Eq, Generic)

instance FromJSON PennPOS where
  parseJSON (J.String "WP$") = pure WPDollar
  parseJSON (J.String "PRP$") = pure PRPDollar
  parseJSON (J.String "-LRB-") = pure LRB
  parseJSON (J.String "-RRB-") = pure RRB
  parseJSON x = J.genericParseJSON jsonOpts x <|> parsePunctuation x
    where
      parsePunctuation (J.String y) = pure (PosPunctuation y)
      parsePunctuation _ = fail "Expecting POS to be a String"

instance ToJSON PennPOS where
  toJSON WPDollar = J.String "WP$"
  toJSON PRPDollar = J.String "PRP$"
  toJSON (PosPunctuation t) = J.String t
  toJSON LRB = J.String "-LRB-"
  toJSON RRB = J.String "-RRB-"
  toJSON x = J.genericToJSON jsonOpts x

-- | See https:\/\/stanfordnlp.github.io\/CoreNLP\/ner.html
data NamedEntity
  = PERSON
  | LOCATION
  | ORGANIZATION
  | MISC
  | MONEY
  | NUMBER
  | ORDINAL
  | PERCENT
  | DATE
  | TIME
  | DURATION
  | SET
  | EMAIL
  | URL
  | CITY
  | STATE_OR_PROVINCE
  | COUNTRY
  | NATIONALITY
  | RELIGION
  | TITLE -- ^ Job title
  | IDEOLOGY
  | CRIMINAL_CHARGE
  | CAUSE_OF_DEATH
  | O -- ^ Not a named entity? TODO: check somehow
  deriving (Show, Eq, Generic)

instance FromJSON NamedEntity where
  parseJSON = J.genericParseJSON jsonOpts

instance ToJSON NamedEntity where
  toJSON = J.genericToJSON jsonOpts

data Token = Token
  { index :: Int
  , word :: Text
  , originalText :: Text
  , lemma :: Text
  , characterOffsetBegin :: Int
  , characterOffsetEnd :: Int
  , pos :: PennPOS
  , ner :: NamedEntity
  , speaker :: Text
  , before :: Text
  , after :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Token where
  parseJSON = J.genericParseJSON jsonOpts

instance ToJSON Token where
  toJSON = J.genericToJSON jsonOpts

data Sentence = Sentence
  { index :: Int
  , parse :: Text
  , basicDependencies :: [Dependency]
  , enhancedDependencies :: [Dependency]
  , enhancedPlusPlusDependencies :: [Dependency]
  , entitymentions :: [Entitymention]
  , tokens :: [Token]
  } deriving (Show, Eq, Generic)

instance FromJSON Sentence where
  parseJSON = J.genericParseJSON jsonOpts

instance ToJSON Sentence where
  toJSON = J.genericToJSON jsonOpts

data Coref = Coref
  { id :: Int
  , text :: Text
  , type_ :: Text
  , number :: Text
  , gender :: Text
  , animacy :: Text
  , startIndex :: Int
  , endIndex :: Int
  , headIndex :: Int
  , sentNum :: Int
  , position :: [Int]
  , isRepresentativeMention :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON Coref where
  parseJSON = J.genericParseJSON jsonOpts

instance ToJSON Coref where
  toJSON = J.genericToJSON jsonOpts

type CorefsId = Text

type Corefs = HashMap CorefsId [Coref]

data Document = Document
  { docId :: Text
  , sentences :: [Sentence]
  , corefs :: Corefs
  } deriving (Show, Eq, Generic)

instance FromJSON Document where
  parseJSON = J.genericParseJSON jsonOpts

instance ToJSON Document where
  toJSON = J.genericToJSON jsonOpts

-- | Parse JSON output of CoreNLP. See 'headlines' source for an example JSON input.
parseJsonDoc :: Text -> Either String Document
parseJsonDoc = J.eitherDecode . S.fromText

-- | Launch CoreNLP with your inputs. This function will put every piece of 'Text' in a separate file, launch CoreNLP subprocess, and parse the results
launchCoreNLP ::
     FilePath -- ^ Path to the directory where you extracted the CoreNLP project
  -> [Text] -- ^ List of inputs
  -> IO [Either String Document] -- ^ List of parsed results
launchCoreNLP fp texts =
  withSystemTempDirectory "corenlp-parser" $ \tempDir -> do
    Prelude.putStrLn $ "Temp dir used is is: " <> tempDir
    tmpFileNames <-
      forM (zip [1 ..] texts) $ \(i :: Integer, txt) -> do
        let fname = ("text-" ++ show i ++ ".txt")
        T.writeFile (tempDir ++ "/" ++ fname) txt
        return fname
    withSystemTempFile "filelist.txt" $ \filelistTxt hfilelistTxt -> do
      Prelude.putStrLn $ "Filelist.txt: " ++ show filelistTxt
      Prelude.putStrLn $ "Temporary files: " ++ show tmpFileNames
      let filesList =
            T.unlines (map (\x -> S.toText (tempDir <> "/" <> x)) tmpFileNames)
      Prelude.putStrLn $ "Filelist.txt filesList: " ++ show filesList
      T.hPutStrLn hfilelistTxt (T.strip filesList)
      hFlush hfilelistTxt
      let spec =
            shell
              ("java --add-modules java.se.ee -cp \"" ++
               fp ++
               "*\" -Xmx2g edu.stanford.nlp.pipeline.StanfordCoreNLP -annotators tokenize,ssplit,pos,lemma,ner,parse,dcoref -outputFormat json -filelist " <>
               filelistTxt)
      (_, _, _, processHandle) <- createProcess spec
      code <- waitForProcess processHandle
      when (code /= ExitSuccess) (error (show code))
      let tmpFileNamesJson = map (<> ".json") tmpFileNames
      results <- forM tmpFileNamesJson $ \fname -> T.readFile fname
      return (map parseJsonDoc results)

headlines :: Text
headlines =
  [r|
{
  "docId": "headlines.txt",
  "sentences": [
    {
      "index": 0,
      "parse": "(ROOT\n  (S\n    (NP\n      (NP (NNP Jersey) (NNP Shore) (NN Season) (CD 6) (NN cast) (POS 's))\n      (NNS salaries))\n    (VP\n      (VP (VBD revealed))\n      (: ;)\n      (NP\n        (NP (JJR More))\n        (PP (IN than)\n          (NP (NNP President) (NNP Obama)))))\n    (. !)))",
      "basicDependencies": [
        {
          "dep": "ROOT",
          "governor": 0,
          "governorGloss": "ROOT",
          "dependent": 8,
          "dependentGloss": "revealed"
        },
        {
          "dep": "compound",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 1,
          "dependentGloss": "Jersey"
        },
        {
          "dep": "compound",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 2,
          "dependentGloss": "Shore"
        },
        {
          "dep": "compound",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 3,
          "dependentGloss": "Season"
        },
        {
          "dep": "nummod",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 4,
          "dependentGloss": "6"
        },
        {
          "dep": "nmod:poss",
          "governor": 7,
          "governorGloss": "salaries",
          "dependent": 5,
          "dependentGloss": "cast"
        },
        {
          "dep": "case",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 6,
          "dependentGloss": "'s"
        },
        {
          "dep": "nsubj",
          "governor": 8,
          "governorGloss": "revealed",
          "dependent": 7,
          "dependentGloss": "salaries"
        },
        {
          "dep": "punct",
          "governor": 8,
          "governorGloss": "revealed",
          "dependent": 9,
          "dependentGloss": ";"
        },
        {
          "dep": "dobj",
          "governor": 8,
          "governorGloss": "revealed",
          "dependent": 10,
          "dependentGloss": "More"
        },
        {
          "dep": "case",
          "governor": 13,
          "governorGloss": "Obama",
          "dependent": 11,
          "dependentGloss": "than"
        },
        {
          "dep": "compound",
          "governor": 13,
          "governorGloss": "Obama",
          "dependent": 12,
          "dependentGloss": "President"
        },
        {
          "dep": "nmod",
          "governor": 10,
          "governorGloss": "More",
          "dependent": 13,
          "dependentGloss": "Obama"
        },
        {
          "dep": "punct",
          "governor": 8,
          "governorGloss": "revealed",
          "dependent": 14,
          "dependentGloss": "!"
        }
      ],
      "enhancedDependencies": [
        {
          "dep": "ROOT",
          "governor": 0,
          "governorGloss": "ROOT",
          "dependent": 8,
          "dependentGloss": "revealed"
        },
        {
          "dep": "compound",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 1,
          "dependentGloss": "Jersey"
        },
        {
          "dep": "compound",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 2,
          "dependentGloss": "Shore"
        },
        {
          "dep": "compound",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 3,
          "dependentGloss": "Season"
        },
        {
          "dep": "nummod",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 4,
          "dependentGloss": "6"
        },
        {
          "dep": "nmod:poss",
          "governor": 7,
          "governorGloss": "salaries",
          "dependent": 5,
          "dependentGloss": "cast"
        },
        {
          "dep": "case",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 6,
          "dependentGloss": "'s"
        },
        {
          "dep": "nsubj",
          "governor": 8,
          "governorGloss": "revealed",
          "dependent": 7,
          "dependentGloss": "salaries"
        },
        {
          "dep": "punct",
          "governor": 8,
          "governorGloss": "revealed",
          "dependent": 9,
          "dependentGloss": ";"
        },
        {
          "dep": "dobj",
          "governor": 8,
          "governorGloss": "revealed",
          "dependent": 10,
          "dependentGloss": "More"
        },
        {
          "dep": "case",
          "governor": 13,
          "governorGloss": "Obama",
          "dependent": 11,
          "dependentGloss": "than"
        },
        {
          "dep": "compound",
          "governor": 13,
          "governorGloss": "Obama",
          "dependent": 12,
          "dependentGloss": "President"
        },
        {
          "dep": "nmod:than",
          "governor": 10,
          "governorGloss": "More",
          "dependent": 13,
          "dependentGloss": "Obama"
        },
        {
          "dep": "punct",
          "governor": 8,
          "governorGloss": "revealed",
          "dependent": 14,
          "dependentGloss": "!"
        }
      ],
      "enhancedPlusPlusDependencies": [
        {
          "dep": "ROOT",
          "governor": 0,
          "governorGloss": "ROOT",
          "dependent": 8,
          "dependentGloss": "revealed"
        },
        {
          "dep": "compound",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 1,
          "dependentGloss": "Jersey"
        },
        {
          "dep": "compound",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 2,
          "dependentGloss": "Shore"
        },
        {
          "dep": "compound",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 3,
          "dependentGloss": "Season"
        },
        {
          "dep": "nummod",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 4,
          "dependentGloss": "6"
        },
        {
          "dep": "nmod:poss",
          "governor": 7,
          "governorGloss": "salaries",
          "dependent": 5,
          "dependentGloss": "cast"
        },
        {
          "dep": "case",
          "governor": 5,
          "governorGloss": "cast",
          "dependent": 6,
          "dependentGloss": "'s"
        },
        {
          "dep": "nsubj",
          "governor": 8,
          "governorGloss": "revealed",
          "dependent": 7,
          "dependentGloss": "salaries"
        },
        {
          "dep": "punct",
          "governor": 8,
          "governorGloss": "revealed",
          "dependent": 9,
          "dependentGloss": ";"
        },
        {
          "dep": "dobj",
          "governor": 8,
          "governorGloss": "revealed",
          "dependent": 10,
          "dependentGloss": "More"
        },
        {
          "dep": "case",
          "governor": 13,
          "governorGloss": "Obama",
          "dependent": 11,
          "dependentGloss": "than"
        },
        {
          "dep": "compound",
          "governor": 13,
          "governorGloss": "Obama",
          "dependent": 12,
          "dependentGloss": "President"
        },
        {
          "dep": "nmod:than",
          "governor": 10,
          "governorGloss": "More",
          "dependent": 13,
          "dependentGloss": "Obama"
        },
        {
          "dep": "punct",
          "governor": 8,
          "governorGloss": "revealed",
          "dependent": 14,
          "dependentGloss": "!"
        }
      ],
      "entitymentions": [
        {
          "docTokenBegin": 3,
          "docTokenEnd": 4,
          "tokenBegin": 3,
          "tokenEnd": 4,
          "text": "6",
          "characterOffsetBegin": 20,
          "characterOffsetEnd": 21,
          "ner": "NUMBER",
          "normalizedNER": "6.0"
        },
        {
          "docTokenBegin": 11,
          "docTokenEnd": 12,
          "tokenBegin": 11,
          "tokenEnd": 12,
          "text": "President",
          "characterOffsetBegin": 58,
          "characterOffsetEnd": 67,
          "ner": "TITLE"
        },
        {
          "docTokenBegin": 12,
          "docTokenEnd": 13,
          "tokenBegin": 12,
          "tokenEnd": 13,
          "text": "Obama",
          "characterOffsetBegin": 68,
          "characterOffsetEnd": 73,
          "ner": "PERSON"
        }
      ],
      "tokens": [
        {
          "index": 1,
          "word": "Jersey",
          "originalText": "Jersey",
          "lemma": "Jersey",
          "characterOffsetBegin": 0,
          "characterOffsetEnd": 6,
          "pos": "NNP",
          "ner": "O",
          "speaker": "PER0",
          "before": "",
          "after": " "
        },
        {
          "index": 2,
          "word": "Shore",
          "originalText": "Shore",
          "lemma": "Shore",
          "characterOffsetBegin": 7,
          "characterOffsetEnd": 12,
          "pos": "NNP",
          "ner": "O",
          "speaker": "PER0",
          "before": " ",
          "after": " "
        },
        {
          "index": 3,
          "word": "Season",
          "originalText": "Season",
          "lemma": "season",
          "characterOffsetBegin": 13,
          "characterOffsetEnd": 19,
          "pos": "NN",
          "ner": "O",
          "speaker": "PER0",
          "before": " ",
          "after": " "
        },
        {
          "index": 4,
          "word": "6",
          "originalText": "6",
          "lemma": "6",
          "characterOffsetBegin": 20,
          "characterOffsetEnd": 21,
          "pos": "CD",
          "ner": "NUMBER",
          "normalizedNER": "6.0",
          "speaker": "PER0",
          "before": " ",
          "after": " "
        },
        {
          "index": 5,
          "word": "cast",
          "originalText": "cast",
          "lemma": "cast",
          "characterOffsetBegin": 22,
          "characterOffsetEnd": 26,
          "pos": "NN",
          "ner": "O",
          "speaker": "PER0",
          "before": " ",
          "after": ""
        },
        {
          "index": 6,
          "word": "'s",
          "originalText": "'s",
          "lemma": "'s",
          "characterOffsetBegin": 26,
          "characterOffsetEnd": 28,
          "pos": "POS",
          "ner": "O",
          "speaker": "PER0",
          "before": "",
          "after": " "
        },
        {
          "index": 7,
          "word": "salaries",
          "originalText": "salaries",
          "lemma": "salary",
          "characterOffsetBegin": 29,
          "characterOffsetEnd": 37,
          "pos": "NNS",
          "ner": "O",
          "speaker": "PER0",
          "before": " ",
          "after": " "
        },
        {
          "index": 8,
          "word": "revealed",
          "originalText": "revealed",
          "lemma": "reveal",
          "characterOffsetBegin": 38,
          "characterOffsetEnd": 46,
          "pos": "VBD",
          "ner": "O",
          "speaker": "PER0",
          "before": " ",
          "after": ""
        },
        {
          "index": 9,
          "word": ";",
          "originalText": ";",
          "lemma": ";",
          "characterOffsetBegin": 46,
          "characterOffsetEnd": 47,
          "pos": ":",
          "ner": "O",
          "speaker": "PER0",
          "before": "",
          "after": " "
        },
        {
          "index": 10,
          "word": "More",
          "originalText": "More",
          "lemma": "more",
          "characterOffsetBegin": 48,
          "characterOffsetEnd": 52,
          "pos": "JJR",
          "ner": "O",
          "speaker": "PER0",
          "before": " ",
          "after": " "
        },
        {
          "index": 11,
          "word": "than",
          "originalText": "than",
          "lemma": "than",
          "characterOffsetBegin": 53,
          "characterOffsetEnd": 57,
          "pos": "IN",
          "ner": "O",
          "speaker": "PER0",
          "before": " ",
          "after": " "
        },
        {
          "index": 12,
          "word": "President",
          "originalText": "President",
          "lemma": "President",
          "characterOffsetBegin": 58,
          "characterOffsetEnd": 67,
          "pos": "NNP",
          "ner": "TITLE",
          "speaker": "PER0",
          "before": " ",
          "after": " "
        },
        {
          "index": 13,
          "word": "Obama",
          "originalText": "Obama",
          "lemma": "Obama",
          "characterOffsetBegin": 68,
          "characterOffsetEnd": 73,
          "pos": "NNP",
          "ner": "PERSON",
          "speaker": "PER0",
          "before": " ",
          "after": ""
        },
        {
          "index": 14,
          "word": "!",
          "originalText": "!",
          "lemma": "!",
          "characterOffsetBegin": 73,
          "characterOffsetEnd": 74,
          "pos": ".",
          "ner": "O",
          "speaker": "PER0",
          "before": "",
          "after": "\n"
        }
      ]
    }
  ],
  "corefs": {
    "1": [
      {
        "id": 1,
        "text": "6",
        "type": "PROPER",
        "number": "SINGULAR",
        "gender": "UNKNOWN",
        "animacy": "INANIMATE",
        "startIndex": 4,
        "endIndex": 5,
        "headIndex": 4,
        "sentNum": 1,
        "position": [
          1,
          3
        ],
        "isRepresentativeMention": true
      }
    ],
    "2": [
      {
        "id": 2,
        "text": "Jersey Shore Season 6 cast 's salaries",
        "type": "NOMINAL",
        "number": "PLURAL",
        "gender": "UNKNOWN",
        "animacy": "INANIMATE",
        "startIndex": 1,
        "endIndex": 8,
        "headIndex": 7,
        "sentNum": 1,
        "position": [
          1,
          1
        ],
        "isRepresentativeMention": true
      }
    ],
    "3": [
      {
        "id": 3,
        "text": "Jersey Shore Season 6 cast 's",
        "type": "PROPER",
        "number": "SINGULAR",
        "gender": "UNKNOWN",
        "animacy": "INANIMATE",
        "startIndex": 1,
        "endIndex": 7,
        "headIndex": 2,
        "sentNum": 1,
        "position": [
          1,
          2
        ],
        "isRepresentativeMention": true
      }
    ],
    "4": [
      {
        "id": 4,
        "text": "More than President Obama",
        "type": "NOMINAL",
        "number": "UNKNOWN",
        "gender": "UNKNOWN",
        "animacy": "INANIMATE",
        "startIndex": 10,
        "endIndex": 14,
        "headIndex": 10,
        "sentNum": 1,
        "position": [
          1,
          4
        ],
        "isRepresentativeMention": true
      }
    ],
    "5": [
      {
        "id": 5,
        "text": "President Obama",
        "type": "PROPER",
        "number": "SINGULAR",
        "gender": "MALE",
        "animacy": "ANIMATE",
        "startIndex": 12,
        "endIndex": 14,
        "headIndex": 13,
        "sentNum": 1,
        "position": [
          1,
          5
        ],
        "isRepresentativeMention": true
      }
    ]
  }
}
|]

test :: IO ()
test = do
  print (parseJsonDoc headlines)
