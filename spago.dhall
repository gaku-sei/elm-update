{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "elm-update"
, dependencies =
  [ "aff"
  , "console"
  , "debug"
  , "effect"
  , "foreign-generic"
  , "generics-rep"
  , "milkis"
  , "node-fs-aff"
  , "node-process"
  , "nonempty"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "read"
  , "spec"
  , "yargs"
  ]
, packages = ./packages.dhall
}
