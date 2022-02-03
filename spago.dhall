{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-test-eunit-discovery"
, dependencies =
  [ "effect"
  , "erl-lists"
  , "erl-modules"
  , "erl-test-eunit"
  , "filterable"
  , "foldable-traversable"
  , "maybe"
  , "free"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
