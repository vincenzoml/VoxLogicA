open System
open System.Text.RegularExpressions

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

let processOddLine line =                
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
    
let processEvenLine line startEndIds =    
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


let p = processOddLine "[fda]----(ba)   [c][]"


[<EntryPoint>]
let main argv =
    let nodes = Set.empty
    let arcs = Set.empty
    let floors =
        IO.File.ReadAllLines(argv.[0]) |>        
        List.ofArray |>        
        List.filter (fun s -> s.StartsWith "//") |>
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
        List.rev    

    let mutable ctx = seq []

    for (fid,floor) in number floors do
        for (lid,line) in number floor do            
            if lid % 2 = 0 then 
                ctx <- processOddLine line                
            else 
                printfn "%A" <| processEvenLine line (Seq.toList (Seq.map (fun (i,(s,f,a,x,z)) -> (s,f,i)) (number ctx)))
            
    0