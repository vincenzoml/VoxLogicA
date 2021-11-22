open System
open System.Text.RegularExpressions

type coord = {| x : int; y : int; z : int|}
type t = 
    {| coord: coord; atoms: seq<char>; xplus : bool; yplus : bool; zplus : bool |}    

let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

let number list =
    Seq.zip (Seq.initInfinite id) list

let splitWhile pred s =
    (List.takeWhile pred s,List.skipWhile pred s)

let splitPred pred s =
    let s' = 
        Seq.fold 
            (fun st el ->
                match (pred el),st with
                | true,_ -> [el]::st
                | false,[] -> [[el]]
                | false,(group::groups) -> (el::group)::groups) 
            []
            s
    Seq.rev (Seq.map Seq.rev s')

let findAllIndexes pred s =
    number s |>
    Seq.fold
        (fun acc (n,el) -> if pred el then n::acc else acc)
        [] |>
    Seq.rev

let rec splitBy2 s =    
    Seq.pairwise s |>
    number |>
    Seq.filter (fun (n,x) -> n % 2 = 0) |>
    Seq.map snd 

let processEvenLine line =                
    let nodoids = splitPred (fun (_,c) -> c = '[' || c = '(' ) (number line)
    seq {
        for nodoid in nodoids do
            let (start,bracket) = Seq.head nodoid 
            let zlink = bracket = '['
            let atomsRest = splitPred (fun (_,c) -> c = ']' || c = ')') nodoid            
            let atoms = Seq.tail <| Seq.head atomsRest
            let finish = 
                Seq.tail atomsRest |>
                Seq.concat |>
                Seq.head |>
                fst
            let xlink = Seq.exists (fun (_,c) -> c = '-') (Seq.concat (Seq.tail atomsRest))
            start,finish,(Seq.map snd atoms),zlink,xlink
    }
    
let processOddLine line startEndIds =    
    let idxs = Seq.toList <| findAllIndexes (fun c -> c = '|') line
    let rec consume idxs tuples acc =
        match (idxs,tuples,acc) with
        | [],_,_ -> acc
        | _,[],_ -> failwith "too many |"
        | idx::idxs',(a,b,id)::pairs',_ ->
            if a <= idx && idx <= b 
            then consume idxs' pairs' (id::acc)
            else consume idxs pairs' acc
    Seq.rev <| consume idxs startEndIds []


let p = processEvenLine "[fda]----(ba)   [c][]"


[<EntryPoint>]
let main argv =
    let nodes = Set.empty
    let arcs = Set.empty
    let floors =
        IO.File.ReadAllLines(argv.[0]) |>        
        List.ofArray |>        
        List.filter (fun s -> not <| Regex.IsMatch(s,"^\s*//")) |>
        List.fold 
            (fun floors line -> 
            match line,floors with
            | Regex @"^\s*-+\s*$" _,_ ->            
                []::floors 
            | Regex @"^\s*$" _,_ ->
                floors 
            | _,[] -> 
                [[line]]
            | _,floor::moreFloors -> 
                (line::floor)::moreFloors) 
            [] |>
        Seq.map Seq.rev |>
        Seq.rev

    let firstPass = seq {        
        for (fid,floor) in number floors do  
            for (lid,(even,odd)) in number (splitBy2 (Seq.append floor (seq {""}))) do   
                let nctx = number <| processEvenLine even                      
                let xtc = Set.ofSeq <| processOddLine odd (Seq.toList (Seq.map (fun (i,(s,f,_,_,_)) -> (s,f,i)) nctx))
                for (cid,(_,_,a,z,x)) in nctx do
                    yield {| coord = {| x = cid; y = lid; z = fid |}; atoms = a; xplus = x; yplus = Set.contains cid xtc; zplus = z |}
    }    
    
    let nx (a : coord) = {| a with x = a.x + 1 |}
    let ny (a : coord) = {| a with y = a.y + 1 |}
    let nz (a : coord) = {| a with z = a.z + 1 |}
    let px (a : coord) = {| a with x = a.x - 1 |}
    let py (a : coord) = {| a with y = a.y - 1 |}
    let pz (a : coord) = {| a with z = a.z - 1 |}
        
    let links = 
        seq { 
            for (x : t) in firstPass do                                                
                for (t,v) in
                    seq {
                        (x.xplus,{| source = x.coord; target = nx x.coord |})
                        (x.yplus,{| source = x.coord; target = ny x.coord |})
                        (x.zplus,{| source = x.coord; target = nz x.coord |})
                    } do
                    yield (t,v)

        } |>
        Seq.filter (fun (t,v) -> t) |>
        Seq.map snd

    let linkSet = Set.ofSeq links 

    let nodes =
        seq {
            for node in firstPass do
                yield {| node with 
                            xminus = Set.contains {| source = px node.coord; target = node.coord |} linkSet
                            yminus = Set.contains {| source = py node.coord; target = node.coord |} linkSet
                            zminus = Set.contains {| source = pz node.coord; target = node.coord |} linkSet |}
        }

    let result = {| nodes = nodes; links = links |}

    printfn "%s" <| Newtonsoft.Json.JsonConvert.SerializeObject(result,Newtonsoft.Json.Formatting.Indented)

    0

