import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen as Gen

import qualified Data.Vector as V
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific as      S
import qualified Data.List as  L
import qualified Data.Aeson as A
import Data.Yaml as            Y
import qualified Data.Bifunctor as B
import qualified Data.Aeson.Key as K

import Data.String.Conversions(cs)

import Document
import Lib

main :: IO ()
main = defaultMain $
  testGroup "Tests" [
      fromYamlTests
    , golden
  ]

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseYaml (Data.Yaml.encode doc) == doc" $
      \doc -> parseYaml (friendlyEncode doc) == Right doc
  ]


fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml" [
      testCase "null (null)" $
        parseYaml "null" @?= Right DNull
    , testCase "null (~)" $
        parseYaml "~" @?= Right DNull
    , testCase "integer" $
        parseYaml "56" @?= Right (DInt 56)
    , testCase "integer" $
        parseYaml "420" @?= Right (DInt 420)
    , testCase "float" $
        parseYaml "6.0" @?= Right (DFloat 6.0)
    , testCase "string with \"" $
        parseYaml "\"foobar\"" @?= Right (DString "foobar")
    , testCase "string with '" $
        parseYaml "'foobar'" @?= Right (DString "foobar")
    , testCase "empty list" $
        parseYaml "[]" @?= Right (DList [])
    , testCase "list with one element" $
        parseYaml "[ 1 ]" @?= Right (DList [DInt 1])
    , testCase "JSON like list" $ parseYaml
        "[[\"not empty\"], 5, [], \"lol\", 'no']"
          @?=
            Right (DList [DList [DString "not empty"], DInt 5, DList [], DString "lol", DString "no"])
    , testCase "JSON like simple list" $ parseYaml
        "[4, 5, 6, \"lol\",'no']"
          @?=
            Right (DList [DInt 4, DInt 5, DInt 6, DString "lol", DString "no"])
    , testCase "JSON like" $ parseYaml "- [key: value, 5, \"lol\", {key: 'value', key1: value1 }]" @?=
        Right (DList [DList [DMap [("key", DString "value")], DInt 5, DString "lol", DMap [("key", DString "value"), ("key1", DString "value1")]]])
    , testCase "List of one" $ parseYaml "- 5" @?= Right (DList [DInt 5])
    , testCase "List of one on new line" $ parseYaml "-\n  5" @?= Right (DList [DInt 5])
    , testCase "List of a couple on new line" $ parseYaml "-\n  5\n-\n  6" @?= Right (DList [DInt 5, DInt 6])
    , testCase "Simple simple list" $ parseYaml (unlines [
        "- asd",
        "- 1",
        "- nice",
        "- 2"
      ]) @?= Right (DList [DString "asd", DInt 1, DString "nice", DInt 2])
    , testCase "Simple list" $ parseYaml (unlines [
        "-",
        "  - lol"
      ])
        @?=
          Right (DList [DList [DString "lol"]])
    , testCase "Simple list" $ parseYaml (unlines [
        "- asd",
        "-",
        "  - lol",
        "- nice",
        "- null"
      ])
        @?=
          Right (DList [DString "asd", DList [DString "lol"], DString "nice", DNull])
    , testCase "Complex list" $ parseYaml (unlines [
        "- 1 1",
        "- 2 2",
        "- foo-123",
        "- foobar",
        "-",
        "  - \"value\"",
        "  -",
        "     - FOO",
        "     - bar",
        "  - 123",
        "-",
        "   -",
        "       - see",
        "-",
        "     -",
        "       - pee",
        "       - hee",
        "     - BAR",
        "- asd"
      ]) @?= Right (
          DList [
            DString "1 1",
            DString "2 2",
            DString "foo-123",
            DString "foobar",
            DList [
              DString "value",
              DList [DString "FOO", DString "bar"],
              DInt 123
            ],
            DList [DList [ DString "see"]],
            DList [DList [
                DString "pee",
                DString "hee"
              ],
              DString "BAR"
            ],
            DString "asd"
          ]
        )
    , testCase "Complex /w only problematic part" $ parseYaml (unlines [
        "-",
        "   -"  ,
        "       - see",
        "-",
        "     -",
        "       - pee",
        "       - hee",
        "     - BAR",
        "- asd"]) @?= Right (DList [
            DList [DList [ DString "see"]],
            DList [ DList [
                DString "pee",
                DString "hee"
              ],
              DString "BAR"
            ],
            DString "asd"
        ])
    , testCase "List ending with indentation" $ parseYaml (unlines [
        "- 5",
        "-",
        "   -",
        "      -",
        "         lll",
        "   - ll"
    ]) @?= Right (
      DList [
        DInt 5,
        DList [DList [DString "lll"], DString "ll"]
      ])
    , testCase "list with 'clif'" $ parseYaml (unlines [
        "-",
        "   -",
        "      lll",
        "- ll"
  ]) @?= Right(DList [DList [DString "lll"], DString "ll"])
    , testCase "Lists with 'clifs'" $ parseYaml (unlines [
        "-",
        "   -"  ,
        "       - see",
        "   -",
        "       -",
        "         - bee",
        "   -",
        "       -",
        "           - high",
        "- 5",
        "-",
        "   -",
        "      -",
        "         lll",
        "- ll"
    ]) @?= Right (
      DList [
        DList [
          DList [DString "see"],
          DList [DList [DString "bee"]],
          DList [DList [DString "high"]]
        ],
        DInt 5,
        DList [DList [DString "lll"]],
        DString "ll"
      ])
    , testCase "Simple mapping" $ parseYaml (unlines [
      "key1: value1",
      "key2: value2",
      "key3: value3"
    ])
      @?=
        Right (DMap [("key1", DString "value1"), ("key2", DString "value2"), ("key3", DString "value3")])
    , testCase "Nested mapping" $ parseYaml (unlines [
      "key:",
      "    key:",
      "      - fml",
      "      - end mi"
    ])
      @?=
        Right (DMap [("key", DMap [("key", DList [DString "fml", DString "end mi"])])])
    , testCase "Mapping with a list" $ parseYaml (unlines [
        "key1: value1",
        "key2:",
        "  - foo",
        "  - bar",
        "key3: value3"
      ])
        @?=
          Right (DMap [("key1", DString "value1"), ("key2", DList [DString "foo", DString "bar"]), ("key3", DString "value3")])
    , testCase "List with a map" $ parseYaml (unlines [
        "- key: stuff",
        "  key1: studd",
        "- 666"
      ])
        @?=
          Right (DList [DMap [("key", DString "stuff"), ("key1", DString "studd")], DInt 666])
    , testCase "List with a nested map" $ parseYaml (unlines [
        "- key:",
        "    key:",
        "      - fml",
        "      - end me",
        "  key1: studd",
        "- 666"
      ])
        @?=            
          Right (DList [
            DMap [
              ("key", 
                DMap [("key", DList [DString "fml", DString "end me"])]),
              ("key1", DString "studd")], 
            DInt 666])
    , testCase "A nested map" $ parseYaml (unlines [
        "key:",
        "  key:",
        "    - fml",
        "    - end me",
        "key1: studd"
      ])
        @?=            
          Right (
            DMap [
              ("key", 
                DMap [("key", DList [DString "fml", DString "end me"])]),
              ("key1", DString "studd")])
    , testCase "List (nl between `-` and map) with a nested map" $ parseYaml (unlines [
          "- ",
          "  key:",
          "    key:",
          "      - fml",
          "      - end me",
          "  key1: studd"
        , "- 666"
      ])
        @?=
          Right (DList [
            DMap [
              ("key", 
                DMap [("key", DList [DString "fml", DString "end me"])]),
              ("key1", DString "studd")], 
            DInt 666])
    , testCase "List (nl between `-` and map) with a nested map" $ parseYaml (unlines [
        "- ",
        "  key:",
        "    key:",
        "      - fml",
        "      - end me",
        "  key1: studd"
      ])
        @?=
          Right (DList [ DMap [
              ("key", 
                DMap [("key", DList [DString "fml", DString "end me"])]),
              ("key1", DString "studd")
              ]])
    , testCase "Single nl scalar" $ parseYaml (unlines [
          "-",
          "  5",
          "- ",
          "   -",
          "      -",
          "         lll",
          "- ll"
    ]) @?= Right (DList[
      DInt 5,
      DList [DList [DString "lll"]],
      DString "ll"
    ])
    , testCase "with whitespace" $ parseYaml "-         5" @?= Right (DList [DInt 5])
    , testCase "with whitespace" $ parseYaml "-         5    " @?= Right (DList [DInt 5])
    , testCase "Simple map of single keyval" $ parseYaml
        "key0: value0" @?= Right (DMap [("key0", DString "value0")])
    , testCase "Simple map of single keyval" $ parseYaml
      (unlines [
        "key0: value0",
        "key1: value1",
        "key2: value2"
      ]) @?= Right (DMap [("key0", DString "value0"), ("key1", DString "value1"), ("key2", DString "value2")])
    , testCase "Simple map" $ parseYaml (unlines [
        "key0:",
        "  - value0",
        "  - value1",
        "key: value"
      ]) @?= Right (DMap [("key0", DList [DString "value0", DString "value1"]), ("key", DString "value")])
    , testCase "Simple map" $ parseYaml (unlines [
        "keys:",
        "  key0: value0",
        "  key1: value1",
        "key: value"
      ]) @?= Right (DMap [("keys", DMap [("key0", DString "value0"), ("key1", DString "value1")]), ("key", DString "value")])
    , testCase "problematic #1" $ parseYaml ( unlines [
          "jy:",
          "- ' 1'"
        ])
      @?=
        Right (DMap [("jy",DList [DString " 1"])])
    , testCase "problematic #2" $ parseYaml (unlines [
          "- - - 0",
          "    - ' 7o5'",
          "  - 7",
          "- -2"
        ])
      @?=
        Right (DList [DList [DList [DInt 0, DString " 7o5"], DInt 7], DInt (-2)])
    , testCase "problematic #2 enhanced" $ parseYaml (unlines [
          "- - - 0",
          "    - ' 7o5'",
          "  - 7",
          "- -2",
          "- - - 0",
          "    - ' 7o5'",
          "  - 7",
          "- -2"
        ])
      @?=
        Right (DList
        [
          DList [DList [DInt 0, DString " 7o5"], DInt 7], DInt (-2),
          DList [DList [DInt 0, DString " 7o5"], DInt 7], DInt (-2)
        ])
    , testCase "problematic #2 enhanced (mapping edition)" $ parseYaml (unlines [
          "- - - 0",
          "    - ' 7o5'",
          "  - 7",
          "- -2",
          "- - - key: val",
          "      key1: val1",
          "  - 7",
          "- -2"
        ])
      @?=
        Right (DList
        [
          DList [DList [DInt 0, DString " 7o5"], DInt 7], DInt (-2),
          DList [DList [DMap [("key", DString "val"), ("key1", DString "val1")]], DInt 7], DInt (-2)
        ])
    , testCase "problematic #3" $ parseYaml (unlines [
          "- bejjKf: -3",
          "  FYVfRg: ' '",
          "  I: []",
          "  Omc: 2 o",
          "- ''"
        ])
      @?=
        Right (DList
        [
          DMap [("bejjKf", DInt (-3)), ("FYVfRg", DString " "), ("I", DList []), ("Omc", DString "2 o")]
          , DString ""
        ])
  ]

instance Arbitrary Document where
  arbitrary = arbitraryDocument

arbitraryDocument :: Gen Document
arbitraryDocument = Gen.oneof [
      arbitraryDString
    -- |Equality between floats is hard, thus it is disabled
    -- , arbitraryDFloat
    , arbitraryDInteger
    , arbitraryDList
    , arbitraryDMap
  ]

arbitraryDString :: Gen Document
arbitraryDString = do
    s <- getSize
    n <- choose (0, min 16 s)
    DString <$> vectorOf n (oneof [arbitraryUpper, arbitraryLower, arbitraryDigit, return ' '])

arbitraryUpper :: Gen Char
arbitraryUpper = chooseEnum ('A', 'Z')

arbitraryLower :: Gen Char
arbitraryLower = chooseEnum ('a', 'z')

arbitraryDigit :: Gen Char
arbitraryDigit = chooseEnum ('0', '9')

arbitraryDInteger :: Gen Document
arbitraryDInteger = DInt <$> arbitrary

-- arbitraryDFloat :: Gen Document
-- arbitraryDFloat = DFloat <$> arbitrary

arbitraryDList :: Gen Document
arbitraryDList = do
    s <- getSize
    n <- choose (0, min 4 s)
    DList <$> vectorOf n arbitraryDocument

arbitraryDMap :: Gen Document
arbitraryDMap = do
    s <- getSize
    n <- choose (0, min 4 s)
    DMap <$> vectorOf n ((,) <$> arbitraryK <*> arbitraryDocument)
    where
        arbitraryK = do
            s <- getSize
            n <- choose (1, min 10 s)
            vectorOf n (oneof [arbitraryUpper, arbitraryLower])

instance ToJSON Document where
    toJSON DNull = A.Null
    toJSON (DInt i) = A.Number (S.scientific (toInteger i) 0)
    toJSON (DFloat f) = A.Number (S.fromFloatDigits f)
    toJSON (DString s) = A.String (cs s)
    toJSON (DList l) = A.Array $ V.fromList $ L.map toJSON l
    toJSON (DMap kvs) = A.Object $ KM.fromList $ L.map (B.bimap K.fromString toJSON) kvs