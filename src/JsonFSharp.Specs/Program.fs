open FSpec.Core.DslV2

let specs =
    describe "Parser" [
        it "works" pending
    ]

let summary =
    [ specs ]
    |> List.map run
    |> List.map getSummary
    |> List.sum
printfn "Summary: %A" summary