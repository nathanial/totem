import Lake
open Lake DSL

package totem where
  version := v!"0.1.0"

require sift from "../../util/sift"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.3"

@[default_target]
lean_lib Totem where
  roots := #[`Totem]

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe totem_tests where
  root := `Tests.Main
