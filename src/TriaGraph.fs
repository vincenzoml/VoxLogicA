module VoxLogicA.TriaGraph

open System.Collections.Generic
open Truth
open FSharp.Json

// Loading data format
type IntSimplex = { id : string; points : int list; atoms : string list }
type IntFileTriaGraph = { numberOfPoints : int; coordinatesOfPoints : int list list; atomNames : string list; simplexes : IntSimplex list }

let private loadFileTriaGraph filename = 
    Json.deserialize<IntFileTriaGraph>(System.IO.File.ReadAllText(filename))


type TriaGraph =
    {   NumPoints : int                         // Number of 0-dimensional simplexes
        NumSimplexes : int                      // Number of simplexes
        NumAtoms : int                          // Number of atoms in the structure
        Points : array<int>                     // The 0-dimensional simplexes
        CoordinatesOfPoint : int[,]             // A 2-dimensional array containing the <x,y,z> coordinates of the points
        ParentsNext : array<Set<int>>           // Given a simplex s of dim d, the (d+1)-simplexes with s as a face
        FacesNext: array<Set<int>>              // Given a simplex s of dim d, the (d-1)-dimensional faces of s
        Parents : array<Set<int>>               // Given a simplex s, the simplexes with s as a face (s included)
        Faces : array<Set<int>>                 // Given a simplex s, the faces of s (s included)
        PointsOfSimplex : array<Set<int>>       // Given a simplex s, the vertices of s
        AtomsOfSimplex : array<Set<int>>        // Given a simplex s, the set of atomic propositions true in s
        SimplexesOfAtom : array<Set<int>>       // Given an atomic proposition ap, the set of simplexes where ap is true
        NameOfAtom : array<string>              // Given an atomic proposition ap, the unique id associated to ap
        AtomOfName : string -> int              // Given the id of an atomic proposition, the corresponding atomic proposition
        SimplexId : array<string>  }            // Given a simplex s, the unique id associated to s

let mkIntFileTriaGraph triaGraph atomName (truth : Truth) =
    let coordPoint point =
        [ for i in [0..2] -> triaGraph.CoordinatesOfPoint.[point, i] ]
    {
        numberOfPoints = triaGraph.NumPoints
        coordinatesOfPoints = [for point in [0..triaGraph.NumPoints-1] -> coordPoint point]
        atomNames = Array.toList( triaGraph.NameOfAtom )
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

    let points = Array.create fg.numberOfPoints 0
    let coordinatesOfPoint = Array2D.create numPoints 3 0 

    let parentsNext = Array.create numSimplexes Set.empty
    let facesNext = Array.create numSimplexes Set.empty

    let Parents = Array.create numSimplexes Set.empty
    let Faces = Array.create numSimplexes Set.empty
    let pointsOfSimplex = Array.create numSimplexes Set.empty

    let atomsOfSimplex = Array.create numSimplexes Set.empty
    let simplexesOfAtom = Array.create numSimplexes Set.empty
    
    let simplexId = Array.create numSimplexes ""


    // let pointDict = Dictionary<string,int>(numPoints)
    let simplexDict = Dictionary<string,int>(numSimplexes)
    // TODO: the following is an enormous upper bound
    // let apDict = Dictionary<string,int>(List.sumBy (fun simplex -> List.length simplex.atoms) fg.simplexes)
    let nameOfAtom = Array.ofList fg.atomNames
    let atomOfName = fun s -> Array.findIndex (fun x -> x = s) nameOfAtom
    let mutable atomsSet = Set.empty


    // let nameOfAtom = Array.create numAtoms List.empty
    // let atomOfName = string -> int
    
    // let newAtomId = 
    //     let mutable curid = 0
    //     fun () -> 
    //         let res = curid 
    //         curid <- curid + 1
    //         res


    // This builds the simplexId, simplexDict, atomsOfSimplex, simplexesOfAtom and atomsSet
    // Moreover, it fills an auxiliary dictionary (hashConstructor) with keys the set of vertices and values the index of the simplex in the array
    let hashConstructor = new Dictionary<int list,int>()
    List.iteri
        (fun idx simplex ->
            hashConstructor.[List.sort simplex.points] <- idx
            simplexId.[idx] <- simplex.id
            simplexDict.[simplex.id] <- idx
            // Subiteration to build atomsOfSimplex, simplexesOfAtom and atomsSet
            List.iter
                (fun atom ->
                    let atomId = atomOfName atom
                    //     try apDict.[atom]
                    //     with :? KeyNotFoundException ->
                    //         let res = newAtomId()
                    //         apDict.[atom] <- res
                    //         res
                    atomsOfSimplex.[idx] <- Set.add atomId atomsOfSimplex.[idx]
                    simplexesOfAtom.[atomId] <- Set.add idx simplexesOfAtom.[atomId]
                    atomsSet <- atomsSet.Add(atomId)                 
                )
                simplex.atoms
        )
        fg.simplexes
    // This builds points and coordinatesOfPoint
    List.iteri
        (fun idx simplex ->
            points.[idx] <- simplex.points.[0]
            List.iteri
                (fun i c ->
                    coordinatesOfPoint.[idx, i] <- c
                    )
                fg.coordinatesOfPoints.[idx]
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
    
    // This builds the Parents, Faces, parentsNext and facesNext
    let rec remove i l =
        match i, l with
        | 0, x::xs -> xs
        | i, x::xs -> x::remove (i - 1) xs
        | i, [] -> failwith "index out of range"
    List.iteri
        (fun idx simplex ->
            // first add the faces
            let pts = List.sort simplex.points
            if List.length pts > 1 then 
                for i in 0 .. (List.length pts - 1) do
                    let pointsOfFace = remove i pts
                    let indexOfFace = hashConstructor.[pointsOfFace]
                    Faces.[idx] <- Set.union Faces.[idx] Faces.[indexOfFace]
                    facesNext.[idx] <- Set.add indexOfFace facesNext.[idx]
            Faces.[idx] <- Set.add idx Faces.[idx]
            // then add the parents
            Parents.[idx] <- Set.add idx Parents.[idx]
            for face in Faces.[idx] do
                Parents.[face] <- Set.add idx Parents.[face]
            for face in facesNext.[idx] do
                parentsNext.[face] <- Set.add idx parentsNext.[face]
        )
        fg.simplexes
        
    
    
    // let isEveryElementContained list1 list2 =
    //     List.forall (fun el -> List.contains el list2) list1
    // List.iteri
    //     (fun idx1 simplex1 ->
    //         List.iteri
    //             (
    //                 fun idx2 simplex2 ->
    //                     let cont = isEveryElementContained simplex1.points simplex2.points
    //                     Parents.[idx2] <-
    //                         if cont then Set.add idx1 Parents.[idx2] else Parents.[idx2]
    //                     Faces.[idx1] <-
    //                         if cont then Set.add idx2 Faces.[idx1] else Faces.[idx1]
    //                     parentsNext.[idx2] <-
    //                         if cont && List.length simplex1.points = (List.length simplex2.points) - 1 then Set.add idx1 parentsNext.[idx2] else parentsNext.[idx2]
    //                     facesNext.[idx1] <-
    //                         if cont && List.length simplex1.points = (List.length simplex2.points) - 1 then Set.add idx2 facesNext.[idx1] else facesNext.[idx1]
    //                     ()
    //             )
    //             fg.simplexes
    //         ()
    //     )
    //     fg.simplexes
    
    // let nameOfAtom =
    //     apDict 
    //         |> Seq.sortBy (fun pair -> pair.Value)
    //         |> Seq.map (fun pair -> pair.Key)
    //         |> Seq.toArray 
    {   NumPoints = numPoints
        NumSimplexes = numSimplexes
        NumAtoms = Set.count atomsSet
        Points = points
        CoordinatesOfPoint = coordinatesOfPoint
        ParentsNext = parentsNext
        FacesNext = facesNext
        Parents = Parents
        Faces = Faces
        PointsOfSimplex = pointsOfSimplex
        AtomsOfSimplex = atomsOfSimplex
        SimplexesOfAtom = simplexesOfAtom
        NameOfAtom = nameOfAtom
        AtomOfName = atomOfName
        SimplexId = simplexId  }    

let loadTriaGraph filename = 
    ErrorMsg.Logger.Debug "Importing json file..."
    let tmp = loadFileTriaGraph filename
    ErrorMsg.Logger.Debug "Processing json file..."
    let res = mkTriaGraph tmp
    ErrorMsg.Logger.Debug "File loaded"
    res


// Computes the *set* of simplexes satysfing the property
let getTriaGraphAp triaGraph ap = 
    let n = triaGraph.AtomOfName ap
    triaGraph.SimplexesOfAtom.[n]


// Compute the topological closure of a set of simplexes S (encoded by truth), i.e. the set S plus SimplexesUp(s) for s in S
let downClosure triaGraph (truth : Truth) : Truth =
    Array.mapi
        (fun idx s -> s || Set.exists (Array.get truth) triaGraph.Parents.[idx])
        truth

// Compute the set S plus SimplexesDown(s) for s in S (topological closure for the Alexandroff topology of the reverse order)
let upClosure triaGraph (truth : Truth) : Truth =
    Array.mapi
        (fun idx s -> s || Set.exists (Array.get truth) triaGraph.Faces.[idx])
        truth

// Computes the topological interior of a set of simplexes S, i.e. the set of simplexes in S not in contact with simplexes outside S
let interior triaGraph (truth : Truth) : Truth =    
    Array.mapi
        (fun idx s -> s && Set.forall (Array.get truth) triaGraph.Faces.[idx])
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
    let others = Set.union triaGraph.Parents.[simplex] triaGraph.Faces.[simplex]
    Set.add simplex others

// Compute the points that can reach a point in "target" passing only through points in "safe".
let reach triaGraph (safe : Truth) (target : Truth) : Truth =
    let intermediate = Array.map2 (&&) (safe) (downClosure triaGraph target) // starting set of the flooding algorithm: simplexes in safe with a face in target
    let visited = Array.copy intermediate
    let rec step (frontier : list<int>) =
        match frontier with
        | [] -> ()  // If the frontier is empty, terminate
        | s::restOfFrontier ->  // If the frontier is not empty, pop a simplex
            intermediate.[s] <- true  // That simplex is in the intermediate set
            let successorsOfS =
                List.filter
                    (fun candidate ->
                        if not visited.[candidate]
                        then
                            visited.[candidate] <- true
                            safe.[candidate]
                        else
                            false
                    )
                    (Set.toList (openStarSimplex triaGraph s))
            step (List.append restOfFrontier successorsOfS)
    let startList = [for i in 0..intermediate.Length-1 do if intermediate.[i] then yield i]
    step startList
    let result = upClosure triaGraph intermediate
    result


    // let rec step (frontier : list<list<int>>) =
    //     match frontier with
    //     | [] -> ()
    //     | []::xs -> step xs
    //     | (x::xs)::ys ->
    //         result.[x] <- true
    //         let closeToX =
    //             List.filter
    //                 (fun candidate ->
    //                     if not visited.[candidate]
    //                     then
    //                         visited.[candidate] <- true
    //                         safe.[candidate]
    //                     else
    //                         false
    //                 )
    //                 (Set.toList (openStarSimplex triaGraph x))
    //         step (closeToX::(xs::ys))
    // let startList = [for i in 0..target.Length-1 do if target.[i] then yield i]
    // step [startList]
    // result



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