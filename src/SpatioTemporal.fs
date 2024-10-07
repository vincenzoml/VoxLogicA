module VoxLogicA.SpatioTemporal

open VoxLogicA.Parser


let eval (c : Command) : Command = 
    match c with
    | _ -> c

let rec flattenSpatioTemporal (syntax : list<Command>) =
    match syntax with
    | [] -> []
    | c::cs -> ((eval c)::(flattenSpatioTemporal cs))
    | _ -> failwith "stub"