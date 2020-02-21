module VoxLogicA.Graph

open System.Collections.Generic

type Node = { id : string; atoms : string list }
type Arc = { source : string; target: string }
type LoadedGraph = { nodes : Node list; arcs : Arc list } 

let private loadGraph filename = 
    FSharp.Json.Json.deserialize<LoadedGraph>(System.IO.File.ReadAllText(filename))    

type private VariableMatrix<'a>(ptr : array<int>,len : array<int>, values : array<'a>) =
    // TODO some assertions are needed in this class
    new (majorSize, totalMinorSize, defaultValue: 'a) = new VariableMatrix<'a>(Array.create majorSize 0, Array.create majorSize 0, Array.create totalMinorSize defaultValue)
    new (majorSize) = new VariableMatrix<'a>(Array.create majorSize 0, Array.create majorSize 0,[||]) 
    member __.Ptr = ptr
    member __.Len = len
    member __.Values = values
    member __.Item
        with get (major,minor) = 
            assert (ptr.[major] + minor < len.[major])
            values.[ptr.[major]+minor]
        and set (major,minor) value =
            assert (ptr.[major] + minor < len.[major])
            values.[ptr.[major]+minor] <- value
    override __.ToString() = 
        let strings = Seq.mapi (fun idx p ->  sprintf "%d -> { ptr = %d, len = %d, values = %s }\n" idx ptr.[idx] len.[idx] (String.concat " " (Seq.map (fun j -> values.[j].ToString()) (seq { ptr.[idx] .. ptr.[idx] + len.[idx] })))) ptr
        String.concat "\n" strings

type private GraphRepresentation =
    // the encoding of arcs is suitable for GPU computing and allocationless computing: 
    // the array ArcsPointer, of size Nodes, contains an index j(x) for each node x. 
    // The array FArcs and BArcs, the same size as the arcs of the graph, contains, starting at j(x), destinations of forward arcs (FArcs) or sources of backward arcs (BArcs) of the node x.
    // The length of each such list of arcs can be deduced by the pointer of the next node
    // Atomic propositions are represented similarly
    {   Nodes : int
        FArcs : VariableMatrix<int>
        BArcs: VariableMatrix<int>
        Atoms: VariableMatrix<int>   }
    override this.ToString() =
        sprintf "Nodes:\n%A\nFArcs:\n%A\nBArcs:\n%A\nAtoms:\n%A\n" this.Nodes this.FArcs this.BArcs this.Atoms

let private reprOfLoaded (lg : LoadedGraph) =
    let numNodes = lg.nodes.Length
    let numArcs = lg.arcs.Length
    let numAtoms = List.sumBy (fun node -> node.atoms.Length) lg.nodes
    let ndApLists = List.map (fun node -> node.atoms) lg.nodes
    let ndAp = Seq.concat ndApLists  
    let ap = List.ofSeq <| Seq.distinct ndAp
    let numAp = ap.Length
    let hashNodes = new Dictionary<string,int>(numNodes)
    let hashAtoms = new Dictionary<'a,int>(numAp)
    List.iteri (fun idx node -> hashNodes.Add(node.id,idx)) lg.nodes
    List.iteri (fun idx atom -> hashAtoms.Add(atom,idx)) ap    
    let fArcs = Array.create numNodes []
    let bArcs = Array.create numNodes []
    List.iter 
        (fun (arc : Arc) -> 
            let s = hashNodes.[arc.source]        
            let t = hashNodes.[arc.target]
            fArcs.[s] <- t::fArcs.[s] 
            bArcs.[t] <- s::fArcs.[t])
        lg.arcs
    let f = new VariableMatrix<_>(numNodes,numArcs,0)
    let b = new VariableMatrix<_>(numNodes,numArcs,0)
    let a = 
        match ap with 
            | [] -> VariableMatrix(numNodes) 
            | x::_ -> new VariableMatrix<_>(numNodes,numAtoms,0)
    for i = 0 to numNodes do        
        f.Len.[i] <- fArcs.[i].Length
        b.Len.[i] <- bArcs.[i].Length
        a.Len.[i] <- lg.nodes.[i].atoms.Length
        if i > 0 then                        
            f.Ptr.[i] <- f.Ptr.[i-1] + f.Len.[i-1]
            b.Ptr.[i] <- b.Ptr.[i-1] + b.Len.[i-1]
            a.Ptr.[i] <- a.Ptr.[i-1] + a.Len.[i-1]
        List.iteri (fun idx value -> f.[i,idx] <- value) fArcs.[i]
        List.iteri (fun idx value -> b.[i,idx] <- value) bArcs.[i]
        List.iteri (fun idx ap -> a.[i,idx] <- hashAtoms.[ap]) lg.nodes.[i].atoms
    {   Nodes = numNodes
        FArcs = f
        BArcs = b
        Atoms = a   }    

type Graph private (gr : GraphRepresentation) =
    new (filename) = Graph(reprOfLoaded(loadGraph(filename)))   

    member __.Save filename = 
        ErrorMsg.Logger.Debug "saving is still not implemented; a printout follows"
        printfn "%A" gr