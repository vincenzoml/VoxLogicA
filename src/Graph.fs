module VoxLogicA.Graph

open System.Collections.Generic
open Truth
open FSharp.Json

type IntNode = { id : string; atoms : string list }
type IntArc = { source : string; target: string }
type IntFileGraph = { nodes : IntNode list; arcs : IntArc list } 

let private loadFileGraph filename = 
    Json.deserialize<IntFileGraph>(System.IO.File.ReadAllText(filename))    

type Graph =
    {   NumNodes : int
        NumAtoms : int
        FArcs : array<list<int>>
        BArcs: array<list<int>>
        AtomsOfNode: array<array<int>>
        NodesOfAtom : array<array<int>>
        NameOfAtom : array<string>
        AtomOfName : string -> int
        NodeId : array<string>  }

let mkIntFileGraph graph atomName (truth : Truth) = {
    nodes = [ 
        for node in 0 .. graph.NumNodes - 1 -> {
            id = graph.NodeId.[node] 
            atoms = 
                let oldAtoms = [ for atom in graph.AtomsOfNode.[node] -> graph.NameOfAtom.[atom] ]
                if truth.[node] then atomName::oldAtoms else oldAtoms
        }
    ]
    arcs = [
        for s in 0 .. graph.NumNodes - 1 do 
            for t in graph.FArcs.[s] -> {                
                source = graph.NodeId.[s]
                target = graph.NodeId.[t]                
            }
    ]
}
        

let saveGraph graph (filename : string) atom truth = 
    let extension = 
        let x = filename.Split(".") 
        x.[x.Length - 1]
    match extension with
    | "json" -> 
        let iGraph = mkIntFileGraph graph atom truth
        System.IO.File.WriteAllText(filename,Json.serialize iGraph)
    | _ -> raise <| CantSaveException(TValuation TBool,extension) 

let private mkGraph (fg : IntFileGraph) =
    
    let numNodes = fg.nodes.Length
    let numAtoms = List.sumBy (fun node -> List.length node.atoms) fg.nodes
    
    let nodeDict = Dictionary<string,int>(numNodes)
    let apDict = Dictionary<string,int>(numAtoms)
    
    let fArcs = Array.create numNodes Set.empty
    let bArcs = Array.create numNodes Set.empty
    let atomsOfNode = Array.create numNodes Set.empty
    let nodesOfAtom = Array.create numAtoms Set.empty
    let nodeId = Array.create numNodes ""
    
    let newAtomId = 
        let mutable curid = 0
        fun () -> 
            let res = curid 
            curid <- curid + 1
            res

    List.iteri 
        (fun idx node -> 
            nodeId.[idx] <- node.id
            nodeDict.[node.id] <- idx
            List.iter
                (fun atom ->
                    let atomId = 
                        try apDict.[atom]
                        with :? KeyNotFoundException ->
                            let res = newAtomId()
                            apDict.[atom] <- res
                            res
                    atomsOfNode.[idx] <- atomsOfNode.[idx].Add atomId                                           
                    nodesOfAtom.[atomId] <- nodesOfAtom.[atomId].Add idx
                )
                node.atoms
        )
        fg.nodes    
    List.iter 
        (fun arc -> 
            let s = nodeDict.[arc.source]
            let t = nodeDict.[arc.target]
            fArcs.[s] <- fArcs.[s].Add t
            bArcs.[t] <- bArcs.[t].Add s
        )    
        fg.arcs
    let nameOfAtom =
        apDict 
            |> Seq.sortBy (fun pair -> pair.Value)
            |> Seq.map (fun pair -> pair.Key)
            |> Seq.toArray        
    {   NumNodes = numNodes
        NumAtoms = numAtoms
        FArcs = Array.map Set.toList fArcs
        BArcs = Array.map Set.toList bArcs
        AtomsOfNode = Array.map Set.toArray atomsOfNode
        NodesOfAtom = Array.map Set.toArray nodesOfAtom
        NameOfAtom = nameOfAtom
        AtomOfName = fun s -> apDict.[s]
        NodeId = nodeId }    

let loadGraph filename = mkGraph(loadFileGraph(filename))

let getAp graph ap = 
    try
        let n = graph.AtomOfName ap
        graph.NodesOfAtom.[n]
    with :? KeyNotFoundException -> 
        [||]

let dilate revArcs (truth : Truth) : Truth = 
    // Computes the closure; the method used requires a "backward" arcs function to be passed (enumerating the nodes of the in-degree of a given node)
    Array.mapi 
        (fun idx v -> v || List.exists (Array.get truth) (revArcs idx))
        truth    

let erode revArcs (truth : Truth) : Truth = 
    Array.mapi 
        (fun idx v -> v && List.forall (Array.get truth) (revArcs idx))
        truth    
    
let fdilate graph truth =
    dilate (Array.get graph.BArcs) truth

let bdilate graph truth = 
    dilate (Array.get graph.FArcs) truth

let berode graph truth =
    erode (Array.get graph.FArcs) truth

let ferode graph truth = 
    erode (Array.get graph.BArcs) truth

let grow (arcs : int -> list<int>) (start : Truth) (condition : int -> bool) =        
    let visited = Array.copy start    
    let result = Array.copy start
    let rec step (frontier : list<list<int>>) = 
        match frontier with
        | [] -> ()
        | []::xs -> step xs
        | (x::xs)::ys ->     
            result.[x] <- true
            let future = 
                List.filter 
                    (fun candidate -> 
                        if not visited.[candidate] 
                        then 
                            visited.[candidate] <- true
                            condition candidate
                        else false)
                    (arcs x)
            step (future::(xs::ys))
    let startList = [for i in 0..start.Length-1 do if start.[i] then yield i]        
    step [startList]
    result

let ftrough graph finish (condition : Truth) : Truth = 
    grow (Array.get graph.BArcs) (Array.map2 (&&) finish condition) (Array.get condition)

let btrough graph finish (condition : Truth) : Truth = 
    grow (Array.get graph.FArcs) (Array.map2 (&&) finish condition) (Array.get condition)