{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "datetime"
    , "debug"
    , "formatters"
    , "prelude"
    , "psci-support"
    , "quickcheck-laws"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
