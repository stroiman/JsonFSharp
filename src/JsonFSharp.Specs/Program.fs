open FSpec.TestDiscovery
open MbUnit.Framework

[<TestFixture>]
type Specs() =
  inherit FSpec.MbUnitWrapper.MbUnitWrapperBase()

[<EntryPoint>]
let main argv =
    System.Reflection.Assembly.GetExecutingAssembly()
    |> runSingleAssembly