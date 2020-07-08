{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "zeta-mapping"
, dependencies =
  [ "console", "effect", "profunctor", "psci-support", "queue", "zeta" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
