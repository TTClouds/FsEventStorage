namespace FsEventStorage.Logging

open Chessie.ErrorHandling

type LogIndexSpec = 
    LogIndexSpecPart list

and LogIndexSpecPart = 
    | SimpleSpecPart
    | MultiSpecPart of byte

module LogIndexSpec =
    let empty : LogIndexSpec = []

    let simple (spec: LogIndexSpec) : LogIndexSpec = 
        List.append spec [ SimpleSpecPart ]

    let multi size (spec: LogIndexSpec) : LogIndexSpec = 
        List.append spec [ MultiSpecPart <| byte size ]

    let partSize = function
        | SimpleSpecPart -> 1
        | MultiSpecPart s -> int s

    let size (spec: LogIndexSpec) = 
        spec |> Seq.sumBy partSize

    let validate 
        maxMultipartSize 
        maxPartsCount
        maxTotalElements
        (spec: LogIndexSpec) = 
        let validatePart (part: LogIndexSpecPart) =
            match part with
            | SimpleSpecPart -> ok ()
            | MultiSpecPart size ->
                if size < 2uy then
                    fail <| sprintf "Part size must not be less than 2"
                elif size > byte maxMultipartSize then
                    fail <| sprintf "Part size must not be greater than %d" maxMultipartSize
                else ok ()

        let parts =
            spec 
            |> List.map validatePart
            |> Trial.collect
            |> Trial.lift ignore

        let partCount =
            if List.length spec > maxPartsCount then
                fail <| sprintf "Spec must not have more than %d parts" maxPartsCount
            else ok()
        
        let totalElements =
            if size spec > maxTotalElements then
                fail <| sprintf "Spec must not have more than %d total elements" maxTotalElements
            else ok()

        [ partCount; parts; totalElements ]
            |> Trial.collect
            |> Trial.lift ignore
        

type LogIndex =
    LogIndexPart list

and LogIndexPart =
    | SimpleIndexPart of uint64
    | MultiIndexPart of uint64 list

module LogIndex =
    let empty : LogIndex = []
