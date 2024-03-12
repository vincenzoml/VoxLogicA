module VoxLogicA.Poset

open System.Collections.Generic
open Truth
open FSharp.Json

//MM: Structure of PoSet for json

type Point =
    { id: int
      atoms: list<string>
      up: list<int> }

type Poset = { points: list<Point> }


let savePoset poset (filename: string) =
    let extension =
        let x = filename.Split(".")
        x.[x.Length - 1]

    match extension with
    | "json" ->        
        System.IO.File.WriteAllText(filename, Json.serialize poset)
    | _ -> raise <| CantSaveException(TValuation TBool, extension)

let loadPoset (filename: string) = 
    let extension = 
        let x = filename.Split(".")
        x.[x.Length - 1]
    match extension with
    | "json" ->
        Json.deserialize<Poset>(System.IO.File.ReadAllText(filename))
    | _ -> raise <| CantLoadException(extension)

