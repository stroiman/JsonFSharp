open FSpec.Core.TestDiscovery

let summary =
    System.Reflection.Assembly.GetExecutingAssembly()
    |> getSpecsFromAssembly
    |> runSpecs
    |> toExitCode