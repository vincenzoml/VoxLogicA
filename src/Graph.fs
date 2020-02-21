module VoxLogicA.Graph

open System.Collections.Generic

type IntNode = { id : string; atoms : string list }
type IntArc = { source : string; target: string }
type IntFileGraph = { nodes : IntNode list; arcs : IntArc list } 

let private loadFileGraph filename = 
    FSharp.Json.Json.deserialize<IntFileGraph>(System.IO.File.ReadAllText(filename))    

type VariableMatrix<'a>(ptr : array<int>,len : array<int>, values : array<'a>) =
    // TODO some assertions are needed in this class
    new (majorSize, totalMinorSize, defaultValue: 'a) = new VariableMatrix<'a>(Array.create majorSize 0, Array.create majorSize 0, Array.create totalMinorSize defaultValue)
    new (majorSize) = new VariableMatrix<'a>(Array.create majorSize 0, Array.create majorSize 0,[||]) 
    member __.Ptr = ptr
    member __.Len = len
    member __.Values = values
    member __.Item
        with get (major,minor) = 
            values.[ptr.[major]+minor]
        and set (major,minor) value =
            values.[ptr.[major]+minor] <- value
    member __.Slice n = 
        seq { for i in ptr.[n] .. ptr.[n] + len.[n] - 1 do yield values.[i] }
    override __.ToString() = 
        let strings = 
            Seq.mapi 
                (fun idx p ->  
                    sprintf "%d -> { ptr = %d, len = %d, values = %s }\n" 
                        idx 
                        p 
                        len.[idx]                         
                        (String.concat " " 
                            (Seq.map 
                                (fun j -> values.[j].ToString()) 
                                (seq { p .. p + len.[idx] - 1 })))
                )                 
                ptr
        String.concat "\n" strings

type Graph =
    // the encoding of arcs is suitable for GPU computing and allocationless computing: 
    // the array ArcsPointer, of size Nodes, contains an index j(x) for each node x. 
    // The array FArcs and BArcs, the same size as the arcs of the graph, contains, starting at j(x), destinations of forward arcs (FArcs) or sources of backward arcs (BArcs) of the node x.
    // The length of each such list of arcs can be deduced by the pointer of the next node
    // Atomic propositions are represented similarly
    {   Nodes : int
        ApNum : string -> int
        NodeAp : int -> Truth.Truth // for each ap, compute an array of size Nodes 
        FArcs : VariableMatrix<int>
        BArcs: VariableMatrix<int>
        Atoms: VariableMatrix<int>   }
    override this.ToString() =
        sprintf "Nodes:\n%A\nFArcs:\n%A\nBArcs:\n%A\nAtoms:\n%A\n" this.Nodes this.FArcs this.BArcs this.Atoms

let private mkGraph (fg : IntFileGraph) =
    let numNodes = fg.nodes.Length
    let numArcs = fg.arcs.Length
    let numAtoms = List.sumBy (fun node -> node.atoms.Length) fg.nodes
    let ndApLists = List.map (fun node -> node.atoms) fg.nodes
    let ndAp = Seq.concat ndApLists  
    let ap = List.ofSeq <| Seq.distinct ndAp
    let numAp = ap.Length
    let hashNodes = new Dictionary<string,int>(numNodes)
    let hashAtoms = new Dictionary<string,int>(numAp)
    List.iteri (fun idx node -> hashNodes.Add(node.id,idx)) fg.nodes
    List.iteri (fun idx atom -> hashAtoms.Add(atom,idx)) ap    
    let fArcs = Array.create numNodes []
    let bArcs = Array.create numNodes []
    List.iter 
        (fun (arc : IntArc) -> 
            let s = hashNodes.[arc.source]        
            let t = hashNodes.[arc.target]
            fArcs.[s] <- t::fArcs.[s] 
            bArcs.[t] <- s::bArcs.[t])
        fg.arcs
    let f = new VariableMatrix<_>(numNodes,numArcs,0)
    let b = new VariableMatrix<_>(numNodes,numArcs,0)
    let a = 
        match ap with 
            | [] -> VariableMatrix(numNodes) 
            | x::_ -> new VariableMatrix<_>(numNodes,numAtoms,0)
    let napAux = VariableMatrix<_>(numAp,numAtoms,0)        
    let nap ap =
        let a = Array.create numNodes false
        Seq.iter (fun node -> a.[node] <- true) (napAux.Slice ap)
        a
    for i = 0 to numNodes - 1 do        
        f.Len.[i] <- fArcs.[i].Length
        b.Len.[i] <- bArcs.[i].Length
        a.Len.[i] <- fg.nodes.[i].atoms.Length        
        if i > 0 then                        
            f.Ptr.[i] <- f.Ptr.[i-1] + f.Len.[i-1]
            b.Ptr.[i] <- b.Ptr.[i-1] + b.Len.[i-1]
            a.Ptr.[i] <- a.Ptr.[i-1] + a.Len.[i-1]
        List.iteri (fun idx value -> f.[i,idx] <- value) fArcs.[i]
        List.iteri (fun idx value -> b.[i,idx] <- value) bArcs.[i]
        List.iteri 
            (fun idx ap -> 
                let h = hashAtoms.[ap]
                a.[i,idx] <- h
                napAux.[h,idx] <- i
                )
            fg.nodes.[i].atoms
    {   Nodes = numNodes
        ApNum = fun s -> hashAtoms.[s]
        NodeAp = nap
        FArcs = f
        BArcs = b
        Atoms = a   }    

let loadGraph filename = mkGraph(loadFileGraph(filename))

let getAp graph ap = 
    try
        let n = graph.ApNum ap
        Some (graph.NodeAp n)
    with :? KeyNotFoundException -> 
        None
    