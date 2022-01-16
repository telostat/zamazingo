import Test.DocTest (doctest)


main :: IO ()
main = doctest
  [ -- Haskell 2021 Extensions List
    "-XBangPatterns"
  , "-XBinaryLiterals"
  , "-XConstrainedClassMethods"
  , "-XConstraintKinds"
  , "-XDeriveDataTypeable"
  , "-XDeriveFoldable"
  , "-XDeriveFunctor"
  , "-XDeriveGeneric"
  , "-XDeriveLift"
  , "-XDeriveTraversable"
  , "-XEmptyCase"
  , "-XEmptyDataDeriving"
  , "-XExistentialQuantification"
  , "-XExplicitForAll"
  , "-XFlexibleContexts"
  , "-XFlexibleInstances"
  , "-XGADTSyntax"
  , "-XGeneralisedNewtypeDeriving"
  , "-XHexFloatLiterals"
  , "-XImportQualifiedPost"
  , "-XInstanceSigs"
  , "-XKindSignatures"
  , "-XMultiParamTypeClasses"
  , "-XNamedFieldPuns"
  , "-XNamedWildCards"
  , "-XNumericUnderscores"
  , "-XPolyKinds"
  , "-XPostfixOperators"
  , "-XRankNTypes"
  , "-XScopedTypeVariables"
  , "-XStandaloneDeriving"
  , "-XStandaloneKindSignatures"
  , "-XTupleSections"
  , "-XTypeApplications"
  , "-XTypeOperators"
  , "-XTypeSynonymInstances"
    -- Project specific language extensions:
  , "-XOverloadedStrings"
  , "-XTemplateHaskell"
    -- Doctest arguments:
  , "-isrc"
  , "src"
  ]
