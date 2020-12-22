module VoxLogicA.TriaGraph

open System.Collections.Generic
open Truth
open FSharp.Json

// Loading data format
type IntSimplex = { id : string; points : int list; atoms : string list }
type IntFileTriaGraph = { numberOfPoints : int; simplexes : IntSimplex list }

let private loadFileTriaGraph filename = 
    Json.deserialize<IntFileTriaGraph>(System.IO.File.ReadAllText(filename))    


type TriaGraph =
    {   NumPoints : int   // Number of 0 simplexes
        NumSimplexes : int
        NumAtoms : int
        Points : array<int>
        FacesDown : array<Set<int>>  // TODO: why not arrays?
        FacesUp: array<Set<int>>
        SimplexesDown : array<Set<int>>
        SimplexesUp : array<Set<int>>
        PointsOfSimplex : array<Set<int>>
        AtomsOfSimplex : array<Set<int>>
        SimplexesOfAtom : array<Set<int>>
        NameOfAtom : array<string>
        AtomOfName : string -> int
        SimplexId : array<string>  }

let mkIntFileTriaGraph triaGraph atomName (truth : Truth) = {
    numberOfPoints = triaGraph.NumPoints
    simplexes = [
        for simplex in 0 .. triaGraph.NumSimplexes - 1 -> {
            id = triaGraph.SimplexId.[simplex]
            points = [for point in triaGraph.PointsOfSimplex.[simplex] -> Array.findIndex (fun p -> p = point) triaGraph.Points]
            atoms = 
                let oldAtoms = [ for atom in triaGraph.AtomsOfSimplex.[simplex] -> triaGraph.NameOfAtom.[atom] ]
                if truth.[simplex] then atomName::oldAtoms else oldAtoms
        }
    ]
}
        

let saveTriaGraph triaGraph (filename : string) atom truth = 
    let extension = 
        let x = filename.Split(".") 
        x.[x.Length - 1]
    match extension with
    | "json" -> 
        let iGraph = mkIntFileTriaGraph triaGraph atom truth
        System.IO.File.WriteAllText(filename,Json.serialize iGraph)
    | _ -> raise <| CantSaveException(TValuation TBool,extension) 



let private mkTriaGraph (fg : IntFileTriaGraph) =
    
    let numPoints = fg.numberOfPoints
    let numSimplexes = fg.simplexes.Length
    // let numAtoms = List.sumBy (fun simplex -> List.length simplex.atoms) fg.simplexes

    let points = Array.create fg.numberOfPoints -1

    let facesDown = Array.create numSimplexes Set.empty
    let facesUp = Array.create numSimplexes Set.empty

    let simplexesDown = Array.create numSimplexes Set.empty
    let simplexesUp = Array.create numSimplexes Set.empty
    let pointsOfSimplex = Array.create numSimplexes Set.empty

    let atomsOfSimplex = Array.create numSimplexes Set.empty
    let simplexesOfAtom = Array.create numSimplexes Set.empty
    
    let simplexId = Array.create numSimplexes ""


    // let pointDict = Dictionary<string,int>(numPoints)
    let simplexDict = Dictionary<string,int>(numSimplexes)
    // TODO: the following is an enormous upper bound
    let apDict = Dictionary<string,int>(List.sumBy (fun simplex -> List.length simplex.atoms) fg.simplexes) 
    let mutable atomsSet = Set.empty


    // let nameOfAtom = Array.create numAtoms List.empty
    // let atomOfName = string -> int
    
    let newAtomId = 
        let mutable curid = 0
        fun () -> 
            let res = curid 
            curid <- curid + 1
            res

    // This builds the simplexId, simplexDict, atomsOfSimplex, simplexesOfAtom and atomsSet
    List.iteri
        (fun idx simplex -> 
            simplexId.[idx] <- simplex.id
            simplexDict.[simplex.id] <- idx
            // Subiteration to build atomsOfSimplex, simplexesOfAtom and atomsSet
            List.iter
                (fun atom ->
                    let atomId = 
                        try apDict.[atom]
                        with :? KeyNotFoundException ->
                            let res = newAtomId()
                            apDict.[atom] <- res
                            res
                    atomsOfSimplex.[idx] <- Set.add atomId atomsOfSimplex.[idx]
                    simplexesOfAtom.[atomId] <- Set.add idx simplexesOfAtom.[atomId]
                    atomsSet <- atomsSet.Add(atomId)
                    ()
                )
                simplex.atoms
        )
        fg.simplexes
    // This builds points
    List.iteri
        (fun idx simplex ->
            points.[idx] <- simplex.points.[0]
        )
        (List.filter
            (fun simplex -> (List.length simplex.points) = 1 )
            fg.simplexes
        )
    // This builds pointsOfSimplex
    List.iteri
        (fun idx simplex ->
            let mutable res = Set.empty
            List.iter
                (fun p ->
                    res <- Set.add p res
                    ()
                )
                simplex.points
            pointsOfSimplex.[idx] <- res
        )
        fg.simplexes
    // This builds the simplexesDown, simplexesUp, facesDown and facesUp
    let isEveryElementContained list1 list2 =
        List.forall (fun el -> List.contains el list2) list1
    List.iteri
        (fun idx1 simplex1 ->
            List.iteri
                (
                    fun idx2 simplex2 ->
                        let cont = isEveryElementContained simplex1.points simplex2.points
                        simplexesDown.[idx2] <-
                            if cont then Set.add idx1 simplexesDown.[idx2] else simplexesDown.[idx2]
                        simplexesUp.[idx1] <-
                            if cont then Set.add idx2 simplexesUp.[idx1] else simplexesUp.[idx1]
                        facesDown.[idx2] <-
                            if cont && List.length simplex1.points = (List.length simplex2.points) - 1 then Set.add idx1 facesDown.[idx2] else facesDown.[idx2]
                        facesUp.[idx1] <-
                            if cont && List.length simplex1.points = (List.length simplex2.points) - 1 then Set.add idx2 facesUp.[idx1] else facesUp.[idx1]
                        ()
                )
                fg.simplexes
            ()
        )    
        fg.simplexes
    let nameOfAtom =
        apDict 
            |> Seq.sortBy (fun pair -> pair.Value)
            |> Seq.map (fun pair -> pair.Key)
            |> Seq.toArray 
    {   NumPoints = numPoints
        NumSimplexes = numSimplexes
        NumAtoms = Set.count atomsSet
        Points = points
        FacesDown = facesDown
        FacesUp = facesUp
        SimplexesDown = simplexesDown
        SimplexesUp = simplexesUp
        PointsOfSimplex = pointsOfSimplex
        AtomsOfSimplex = atomsOfSimplex
        SimplexesOfAtom = simplexesOfAtom
        NameOfAtom = nameOfAtom
        AtomOfName = fun s -> apDict.[s]
        SimplexId = simplexId  }    

let loadTriaGraph filename = mkTriaGraph(loadFileTriaGraph(filename))


// Computes the *set* of simplexes satysfing the property
let getTriaGraphAp triaGraph ap = 
    let n = triaGraph.AtomOfName ap
    triaGraph.SimplexesOfAtom.[n]


// Compute the topological closure of a set of simplexes S (encoded by truth), i.e. the set S plus SimplexesUp(s) for s in S
let downClosure triaGraph (truth : Truth) : Truth =
    Array.mapi
        (fun idx s -> s || Set.exists (Array.get truth) triaGraph.SimplexesDown.[idx])
        truth

// Compute the set S plus SimplexesDown(s) for s in S (topological closure for the Alexandroff topology of the reverse order)
let upClosure triaGraph (truth : Truth) : Truth =
    Array.mapi
        (fun idx s -> s || Set.exists (Array.get truth) triaGraph.SimplexesUp.[idx])
        truth

// Computes the open star of a set of simplexes, i.e. the dilation in the graph associated to the triangulation
// TODO: make it local, like downClosure and upClosure. What is more efficient?
let openStar triaGraph (truth : Truth) : Truth = 
    Array.map2
        (||)
        (downClosure triaGraph truth)
        (upClosure triaGraph truth)

// Compute the open star of a single simplex as a set
let openStarSimplex triaGraph simplex : Set<int> =
    let others = Set.union triaGraph.SimplexesDown.[simplex] triaGraph.SimplexesUp.[simplex]
    Set.add simplex others


// Computes the topological interior of a set of simplexes S, i.e. the set of simplexes in S not in contact with simplexes outside S
let interior triaGraph (truth : Truth) : Truth =    
    Array.mapi
        (fun idx s -> s && Set.forall (Array.get truth) triaGraph.SimplexesUp.[idx] && Set.forall (Array.get truth) triaGraph.SimplexesDown.[idx])
        truth


// Compute the points that can reach a point in "target" passing only through points in "safe".
// (If the initial point is in target, then the condition holds; if the starting point is neither in safe nor in target, then the condition does not hold)
let reach triaGraph (safe : Truth) (target : Truth) : Truth =
    let visited = Array.copy target
    let result = Array.copy target
    let rec step (frontier : list<list<int>>) =
        match frontier with
        | [] -> ()
        | []::xs -> step xs
        | (x::xs)::ys ->
            result.[x] <- true
            let closeToX =
                List.filter
                    (fun candidate ->
                        if not visited.[candidate]
                        then
                            visited.[candidate] <- true
                            safe.[candidate]
                        else
                            false
                    )
                    (Set.toList (openStarSimplex triaGraph x))
            step (closeToX::(xs::ys))
    let startList = [for i in 0..target.Length-1 do if target.[i] then yield i]
    step [startList]
    result



//// CODE FROM A PREVIOUS IMPLEMENTATION
    
// let fdilate graph truth =
//     dilate (Array.get graph.BArcs) truth

// let bdilate graph truth = 
//     dilate (Array.get graph.FArcs) truth

// let berode graph truth =
//     erode (Array.get graph.FArcs) truth

// let ferode graph truth = 
//     erode (Array.get graph.BArcs) truth

// let grow (arcs : int -> list<int>) (start : Truth) (condition : int -> bool) =        
//     let visited = Array.copy start    
//     let result = Array.copy start
//     let rec step (frontier : list<list<int>>) = 
//         match frontier with
//         | [] -> ()
//         | []::xs -> step xs
//         | (x::xs)::ys ->     
//             result.[x] <- true
//             let future = 
//                 List.filter 
//                     (fun candidate -> 
//                         if not visited.[candidate] 
//                         then 
//                             visited.[candidate] <- true
//                             condition candidate
//                         else false)
//                     (arcs x)
//             step (future::(xs::ys))
//     let startList = [for i in 0..start.Length-1 do if start.[i] then yield i]        
//     step [startList]
//     result

// let ftrough graph finish (condition : Truth) : Truth = 
//     grow (Array.get graph.BArcs) (Array.map2 (&&) finish condition) (Array.get condition)

// let btrough graph finish (condition : Truth) : Truth = 
//     grow (Array.get graph.FArcs) (Array.map2 (&&) finish condition) (Array.get condition)