module FsEventStorage.Logging.Tests

open NUnit.Framework
open FsUnit
open Chessie.ErrorHandling

let okResult: Result<unit, string> = ok()
let failResults msgs: Result<unit, string> = Bad msgs
let failResult msg = failResults [msg]

[<Test>]
let ``LogIndexSpec.empty should be an empty list``() =
    LogIndexSpec.empty
    |> should equal []

[<Test>]
let ``LogIndexSpec.simple should add a SimpleSpecPart``() =
    LogIndexSpec.empty
    |> LogIndexSpec.simple
    |> should equal [ SimpleSpecPart ]

[<Test>]
let ``LogIndexSpec.multi should add a MultiSpecPart``() =
    LogIndexSpec.empty
    |> LogIndexSpec.multi 3
    |> should equal [ MultiSpecPart 3uy ]

[<Test>]
let ``LogIndexSpec with many parts should work as expected``() =
    LogIndexSpec.empty
    |> LogIndexSpec.multi 3
    |> LogIndexSpec.simple
    |> LogIndexSpec.multi 2
    |> should equal [ MultiSpecPart 3uy; SimpleSpecPart; MultiSpecPart 2uy ]

[<Test>]
let ``LogIndexSpec.validate with correct parts should validate successfuly``() =
    LogIndexSpec.empty
    |> LogIndexSpec.multi 3
    |> LogIndexSpec.simple
    |> LogIndexSpec.multi 2
    |> LogIndexSpec.validate 4 8 16
    |> should equal okResult

[<Test>]
let ``LogIndexSpec.validate with too low multipart should be invalid``() =
    LogIndexSpec.empty
    |> LogIndexSpec.multi 1
    |> LogIndexSpec.validate 4 8 16
    |> should equal (failResult "Part size must not be less than 2")

[<Test>]
let ``LogIndexSpec.validate with too high multipart should be invalid``() =
    LogIndexSpec.empty
    |> LogIndexSpec.multi 5
    |> LogIndexSpec.validate 4 8 16
    |> should equal (failResult "Part size must not be greater than 4")

[<Test>]
let ``LogIndexSpec.validate with too many parts should be invalid``() =
    (LogIndexSpec.empty, seq {1 .. 9})
    ||> Seq.fold (fun s _ -> LogIndexSpec.simple s)
    |> LogIndexSpec.validate 4 8 16
    |> should equal (failResult "Spec must not have more than 8 parts")

[<Test>]
let ``LogIndexSpec.validate with too many elements should be invalid``() =
    (LogIndexSpec.empty, seq {1 .. 5})
    ||> Seq.fold (fun s _ -> LogIndexSpec.multi 4 s)
    |> LogIndexSpec.validate 4 8 16
    |> should equal (failResult "Spec must not have more than 16 total elements")



[<Test>]
let ``LogIndex.empty should be an empty list``() =
    LogIndex.empty
    |> should equal []
