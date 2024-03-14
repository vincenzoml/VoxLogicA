module VoxLogicA.Truth

// TODO: use Array.Parallel (or Hapoc's equivalent)
type Truth = array<bool>

let empty = Array.empty
let TT n = Array.create n true
let FF n = Array.create n false
let BConst n v = Array.create n v
let And = Array.map2 (&&)  
let Or = Array.map2 (||) 
let Not = Array.map not

let iterate = Array.iter

// let saveTruth =
//         let saveLock = ref ()
//         fun (filename : string) (truth : Truth) ->
//             lock            
//                 saveLock
//                 (fun () ->
//                     ErrorMsg.Logger.Debug <| sprintf "saving (filename: %s) is still not implemented; a printout follows" filename
//                     ErrorMsg.Logger.Debug <| sprintf "%A" (Array.mapi (fun idx v -> (idx,v)) truth))
