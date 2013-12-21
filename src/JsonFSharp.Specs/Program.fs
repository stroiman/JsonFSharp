open FSpec.Core.DslV2

let summary =
    [ PrimitiveTypesSpec.specs;
      ErrorHandlingSpecs.specs ]
    |> List.map run
    |> List.map getSummary
    |> List.sum
printfn "Summary: %A" summary