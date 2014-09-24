open FSpec.TestDiscovery

[<EntryPoint>]
let main argv =
    System.Reflection.Assembly.GetExecutingAssembly()
    |> runSingleAssembly