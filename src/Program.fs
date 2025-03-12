// Copyright 2018 Vincenzo Ciancia.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
//
// A copy of the license is available in the file "Apache_License.txt".
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// ltsinfo
// ltsgraph

module VoxLogicA.Main

open System.Reflection
open Argu
open FSharp.Json

exception CommandLineException with
    override __.Message = "Invalid arguments. Usage:\nVoxLogicA <FILENAME>"

type LoadFlags = { fname: string; numCores: int }

type CmdLine =
    | Ops
    | Framefile of string
    | Convert of string * string        
    | FakeConversion
    | [<MainCommand;UniqueAttribute>] Filename of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Ops -> "display a list of all the internal operators, with their types and a brief description"
            | Filename _ -> "VoxLogicA session file"
            | Framefile _ -> "MCRL2 frame file"
            | FakeConversion -> "Write much less info to the destination file, used just for measurement"
            | Convert _ -> "Convert from/to image, json, aut (mcrl2), optionally generating a mcrl2 frame"

let (|Img|_|) (s: string) =
    match System.IO.Path.GetExtension s with
    | (".png"
    | ".jpg"
    | ".bmp"
    | ".nii.gz"
    | ".nii") -> Some Img
    | _ -> None

let (|JSon|_|) (s: string) =
    match System.IO.Path.GetExtension s with
    | ".json" -> Some JSon
    | _ -> None

let (|Aut|_|) (s: string) =
    match System.IO.Path.GetExtension s with
    | ".aut" -> Some Aut
    | _ -> None

let writeFrame (atoms: System.Collections.Generic.HashSet<string>) (mcrlFrameFileName: string) =
    let swFrame = new System.IO.StreamWriter(mcrlFrameFileName)
    swFrame.AutoFlush <- false
    swFrame.Write $"act \n"
    swFrame.Write (String.concat ",\n" atoms)
    swFrame.Write ";\n"
    swFrame.Write "init delta;\n"
    swFrame.Close()

let graphToAut (j: Graph.IntFileGraph) (full: bool) (autFileName: string) (mcrlFrameFileName: string option) =
    ErrorMsg.Logger.Debug $"graphToAut {autFileName} {mcrlFrameFileName}" 
    let nodeOfId = new System.Collections.Generic.Dictionary<_, _>()
    let ids = new System.Collections.Generic.HashSet<int>()
    let sortedNodes = j.nodes |> List.sortBy (fun n -> 
        let intId = int n.id
        let _ = ids.Add intId
        intId
    )
    // check that ids are consecutive and start from 0
    assert (Seq.forall (fun i -> ids.Contains i) { 0 .. sortedNodes.Length - 1 })
    
    List.iteri (fun i (v: Graph.IntNode) -> nodeOfId[v.id] <- {| index = i; node = v |}) sortedNodes
    let n = sortedNodes.Length
    let transitions = new System.Collections.Generic.HashSet<_>()
    let addTransition t = ignore <| transitions.Add t

    let mutable changeFound = false

    for a in j.arcs do
        let src = nodeOfId[a.source]
        let tgt = nodeOfId[a.target]
        let condition = List.sort src.node.atoms = List.sort tgt.node.atoms // Comparison
        addTransition (src.index, 
            (if condition 
                then "tau" 
                else 
                    changeFound <- true 
                    "change"), 
            tgt.index)

        if full then
            addTransition (tgt.index + n, 
                (if condition 
                    then "tau" 
                    else 
                        changeFound <- true 
                        "change"), src.index + n)

    let atoms = new System.Collections.Generic.HashSet<string>()

    for kv in nodeOfId do
        let idx = kv.Value.index

        for atom in kv.Value.node.atoms do
            let _ = atoms.Add atom 
            addTransition (idx, atom, idx)

        if full then
            addTransition (idx, "l1", idx + n)
            addTransition (idx + n, "l2", idx)

    match mcrlFrameFileName with
    | Some mcrlFrameFileName ->
        if full then 
            let _ = atoms.Add "l1"
            let _ = atoms.Add "l2"
            let _ = atoms.Add "change"
            ()            
        if changeFound then
            let _ = atoms.Add "change"
            ()
        writeFrame atoms mcrlFrameFileName
    | None -> ()

    let swAut = new System.IO.StreamWriter(autFileName)
    swAut.AutoFlush <- false
    swAut.Write $"des (0,{transitions.Count},{if full then 2 * n else n})\n"

    for t in transitions do
        swAut.WriteLine(t.ToString())

    swAut.Close()


let img2DUInt8ToGraph fakeConversion (img: SITKUtil.VoxImage) (autFileName: string) (mcrlFrameFileName: string option) =
    assert (img.BufferType = SITKUtil.PixelType.UInt8)
    assert (img.Dimension = 2)
    let size = [| img.Size[0]; img.Size[1] |]
    let w = size[0]
    let h = size[1]
    let nodes = img.NPixels
    let ncomps = img.NComponents

    let arcs =
        (nodes * 9)
        - (4 * 5)
        - (2 * (img.Size[0] - 2) * 3)
        - (2 * (img.Size[1] - 2) * 3)

    let displacementsALL =
        [| -(w + 1)
           -w
           -(w - 1)
           -1
           1
           w - 1
           w
           w + 1 |]

    let displacementsUL = [| 1; w; w + 1 |]
    let displacementsU = [| -1; 1; w - 1; w; w + 1 |]
    let displacementsUR = [| -1; w - 1; w |]
    let displacementsL = [| -w; -(w - 1); 1; w; w + 1 |]
    let displacementsR = [| -(w + 1); -w; -1; w - 1; w |]
    let displacementsDL = [| -w; -(w - 1); 1 |]
    let displacementsD = [| -(w + 1); -w; -(w - 1); -1; 1 |]
    let displacementsDR = [| -(w + 1); -w; -1 |]

    let displacements i =
        let m = i % w
        let (u, l, r, d) = (i < w, m = 0, m = w - 1, (h - 1) * w <= i) // position of the node i: upper row, leftmost column, rightmost column, last row

        match (u, l, r, d) with
        | (false, false, false, false) -> displacementsALL
        | (true, true, false, _) -> displacementsUL
        | (true, false, false, _) -> displacementsU
        | (true, _, true, _) -> displacementsUR
        | (false, true, _, false) -> displacementsL
        | (false, _, true, false) -> displacementsR
        | (_, true, _, true) -> displacementsDL
        | (_, false, false, true) -> displacementsD
        | (_, false, true, true) -> displacementsDR

    let fname =
        if not fakeConversion then
            autFileName
        else
            System.IO.Path.GetTempFileName()

    use fs = System.IO.File.Open(fname, System.IO.FileMode.Create)
    use sw = new System.IO.StreamWriter(fs, System.Text.Encoding.ASCII, 1048576)
    sw.AutoFlush <- false
    sw.WriteLine $"des (0,{arcs},{nodes})"
    let mutable fakeConversionUtilizer = 0

    let atoms = new System.Collections.Generic.HashSet<string>()
    let mutable changeFound = false

    img.GetBufferAsUInt8 (fun buf ->
        for i = 0 to nodes - 1 do
            let baseidx = i * ncomps
            let r = buf.UGet baseidx
            let g = buf.UGet <| baseidx + 1
            let b = buf.UGet <| baseidx + 2

            if not fakeConversion then
                // let fmt x : string =
                //     System.String.Format("{0:X2}", (x: uint8))

                let atom = $"c{r:X2}{g:X2}{b:X2}"
                let _ = atoms.Add atom

                sw.Write "("
                sw.Write i
                sw.Write ", "
                sw.Write atom
                sw.Write ","
                sw.Write i
                sw.WriteLine ")"

            for d in displacements i do
                let target = i + d
                let tbaseidx = target * ncomps
                let tr = buf.UGet tbaseidx
                let tg = buf.UGet <| tbaseidx + 1
                let tb = buf.UGet <| tbaseidx + 2

                let label =
                    if r = tr && g = tg && b = tb then
                        "tau"
                    else
                        changeFound <- true
                        "change"

                if not fakeConversion then
                    sw.Write "("
                    sw.Write i
                    sw.Write ","
                    sw.Write label
                    sw.Write ","
                    sw.Write target
                    sw.WriteLine ")"
                else
                    fakeConversionUtilizer <- label.Length)

    if changeFound then
        let _ = atoms.Add "change"
        ()

    match mcrlFrameFileName with
    | Some mcrlFrameFileName ->
        writeFrame atoms mcrlFrameFileName
    | None -> ()

    if fakeConversion then
        ErrorMsg.Logger.Debug $"Fake conversion done (unuseful integer: {fakeConversionUtilizer})"

    sw.Close()

let imgToGraph (img: SITKUtil.VoxImage) =
    let dim = img.Size.Length

    let s =
        if dim >= 3 then
            img.Size
        else
            Array.append (img.Size) [| 1 |]

    let mkNode (buf: NativeArray<_>) i j optk =
        let id, start =
            match optk with
            | None -> string (i, j), img.NComponents * (i + (j * s.[0]))
            | Some k -> string (i, j, k), img.NComponents * (i + (j * s.[0])) + (k * s.[1])

        { Graph.id = id
          Graph.atoms =
            [ let r = buf.Get start
              let g = buf.Get(start + 1)
              let b = buf.Get(start + 2)
              sprintf "#%02X%02X%02X" (int r) (int g) (int b) ] }


    img.GetBufferAsFloat (fun buf ->
        { Graph.nodes =
            List.ofSeq
            <| seq {
                for i in 0 .. (s.[0] - 1) do
                    for j in 0 .. (s.[1] - 1) do
                        if dim < 3 then
                            mkNode buf i j None
                        else
                            for k in 0 .. (s.[2] - 1) do
                                mkNode buf i j (Some k)
            }
          Graph.arcs =
            List.ofSeq
            <| seq {
                for i in 0 .. (s[0] - 1) do
                    for j in 0 .. (s[1] - 1) do
                        if dim < 3 then
                            for a in -1 .. 1 do
                                for b in -1 .. 1 do
                                    let m = i + a
                                    let n = j + b

                                    if a <> 0 || b <> 0 then
                                        if 0 <= m && m < s[0] && 0 <= n && n < s[1] then
                                            { Graph.source = string (i, j)
                                              Graph.target = string (m, n) }
                        else
                            for k in 0 .. (s[2] - 1) do
                                for a in -1 .. 1 do
                                    for b in -1 .. 1 do
                                        for c in -1 .. 1 do
                                            let m = i + a
                                            let n = j + b
                                            let o = k + c

                                            if a <> 0 || b <> 0 || c <> 0 then
                                                if 0 <= m
                                                   && m < s[0]
                                                   && 0 <= n
                                                   && n < s[1]
                                                   && 0 <= 0
                                                   && o < s[2] then
                                                    { Graph.source = string (i, j, k)
                                                      Graph.target = string (m, n, o) }
            } })

let imgToAut (img: VoxLogicA.SITKUtil.VoxImage) (s2: string) =
    let dim = img.Size.Length

    let s =
        if dim >= 3 then
            img.Size
        else
            Array.append (img.Size) [| 1 |]

    let mkNode (buf: NativeArray<_>) i j optk =
        let id, start =
            match optk with
            | None -> string (i, j), img.NComponents * (i + (j * s.[0]))
            | Some k -> string (i, j, k), img.NComponents * (i + (j * s.[0])) + (k * s.[1])

        { Graph.id = id
          Graph.atoms =
            [ let r = buf.Get start
              let g = buf.Get(start + 1)
              let b = buf.Get(start + 2)
              sprintf "#%02X%02X%02X" (int r) (int g) (int b) ] }


    img.GetBufferAsFloat (fun buf ->
        { Graph.nodes =
            List.ofSeq
            <| seq {
                for i in 0 .. (s.[0] - 1) do
                    for j in 0 .. (s.[1] - 1) do
                        if dim < 3 then
                            mkNode buf i j None
                        else
                            for k in 0 .. (s.[2] - 1) do
                                mkNode buf i j (Some k)
            }
          Graph.arcs =
            List.ofSeq
            <| seq {
                for i in 0 .. (s[0] - 1) do
                    for j in 0 .. (s[1] - 1) do
                        if dim < 3 then
                            for a in -1 .. 1 do
                                for b in -1 .. 1 do
                                    let m = i + a
                                    let n = j + b

                                    if a <> 0 || b <> 0 then
                                        if 0 <= m && m < s[0] && 0 <= n && n < s[1] then
                                            { Graph.source = string (i, j)
                                              Graph.target = string (m, n) }
                        else
                            for k in 0 .. (s[2] - 1) do
                                for a in -1 .. 1 do
                                    for b in -1 .. 1 do
                                        for c in -1 .. 1 do
                                            let m = i + a
                                            let n = j + b
                                            let o = k + c

                                            if a <> 0 || b <> 0 || c <> 0 then
                                                if 0 <= m
                                                   && m < s[0]
                                                   && 0 <= n
                                                   && n < s[1]
                                                   && 0 <= 0
                                                   && o < s[2] then
                                                    { Graph.source = string (i, j, k)
                                                      Graph.target = string (m, n, o) }
            } })

open System.Text.RegularExpressions

let autToGraph s =
    let partition predicate f1 f2 source =
        let map =
            source
                |> Seq.groupBy predicate
                |> Map.ofSeq
        let get flag =
            map
                |> Map.tryFind flag
                |> Option.defaultValue Seq.empty
        (Seq.map f1 (get true)),(Seq.map f2 (get false))
    let lines = System.IO.File.ReadLines(s)
    let oneArg = "\s*\"?\s*([^\s\"]*)\s*\"?\s*" // Replace the "c" with whatever
    let triple = $"\s*\({oneArg},{oneArg},{oneArg}\)\s*"

    let (startState, transitions, numStates) =
        let t = Regex.Match(Seq.head lines, $"\s*des\s*{triple}")
        (int t.Groups[1].Value, int t.Groups[2].Value, int t.Groups[3].Value)

    let parseTransition s =
        let m = Regex.Match(s, triple)
        let s = m.Groups[1].Value
        let t = m.Groups[3].Value
        let l = m.Groups[2].Value
        if s = t then
            Choice1Of2 { 
                Graph.id = s // ((int s) + startState).ToString()
                Graph.atoms = [ l ] }
        else
            Choice2Of2 { 
                Graph.source = s
                Graph.target = t
            }

    let unChoice1 x = match x with Choice1Of2 x -> x | _ -> failwith "Invalid transition"
    let unChoice2 x = match x with Choice2Of2 x -> x | _ -> failwith "Invalid transition"
    
    let (nodes,arcs) = 
        Seq.map parseTransition (Seq.tail lines) |>
        partition (fun x -> match x with Choice1Of2 _ -> true | _ -> false) unChoice1 unChoice2 
    { Graph.nodes = List.ofSeq nodes; Graph.arcs = List.ofSeq arcs}



[<EntryPoint>]
let main (argv: string array) =
    let name = Assembly.GetEntryAssembly().GetName()
    let version = name.Version

    let informationalVersion =
        ((Assembly
            .GetEntryAssembly()
            .GetCustomAttributes(typeof<AssemblyInformationalVersionAttribute>, false).[0])
        :?> AssemblyInformationalVersionAttribute)
            .InformationalVersion

    ErrorMsg.Logger.Debug(sprintf "%s %s" name.Name informationalVersion)

    if version.Revision <> 0 then
        ErrorMsg.Logger.Warning(
            sprintf
                "You are using a PRERELEASE version of %s. The most recent stable release is %d.%d.%d."
                name.Name
                version.Major
                version.Minor
                version.Build
        )

    try
        let cmdLineParser =
            ArgumentParser.Create<CmdLine>(programName = name.Name, errorHandler = ProcessExiter())

        let parsed = cmdLineParser.Parse argv

        if parsed.Contains Convert then
            let fakeConversion = parsed.Contains FakeConversion
            let s1, s2 = parsed.GetResult Convert

            match s1, s2 with            
            | JSon, Aut ->
                let j = Graph.loadFileGraph (s1)
                if parsed.Contains Framefile then
                    let framefile = parsed.GetResult Framefile
                    graphToAut j false s2 (Some framefile)
                else
                    graphToAut j false s2 None
                exit 0
            | Img, Aut ->
                let (imgf, autf) = (s1, s2)
                ErrorMsg.Logger.Debug $"Loading {imgf}"
                let img = new SITKUtil.VoxImage(imgf)

                if img.BufferType = SITKUtil.PixelType.UInt8
                   && img.Dimension = 2
                   && img.NComponents >= 3 then
                    ErrorMsg.Logger.Debug "Using optimized 2D Uint8 transform"
                    if parsed.Contains Framefile then
                        let framefile = parsed.GetResult Framefile
                        img2DUInt8ToGraph fakeConversion img s2 (Some framefile)
                    else
                        img2DUInt8ToGraph fakeConversion img s2 None
                else
                    ErrorMsg.Logger.Debug
                        $"Using non-optimized transform (dimension: {img.Dimension}, type: {img.BufferType})"

                    let j = imgToGraph img
                    if parsed.Contains Framefile then
                        let framefile = parsed.GetResult Framefile
                        graphToAut j false autf (Some framefile)
                    else
                        graphToAut j false autf None
                    exit 0

            | JSon, Img->
                let j = Graph.loadFileGraph (s1)

                let parseTriple s =
                    let reg =
                        new System.Text.RegularExpressions.Regex "^\s*\((\s*[0-9]+\s*),(\s*[0-9]+\s*),(\s*[0-9]+\s*)\)\s*$"

                    let m = reg.Match s

                    if m.Success then
                        [| int m.Groups[1].Value
                           int m.Groups[2].Value
                           int m.Groups[3].Value |]
                    else
                        failwith $"Not a triple: {s}"

                let parseColour s =
                    let reg =
                        new System.Text.RegularExpressions.Regex "^\s*#([0-9a-fA-F][0-9a-fA-F])([0-9a-fA-F][0-9a-fA-F])([0-9a-fA-F][0-9a-fA-F])\s*$"

                    let m = reg.Match s

                    let f x =
                        System.Int32.Parse(x, System.Globalization.NumberStyles.HexNumber)

                    if m.Success then
                        [| f m.Groups[1].Value
                           f m.Groups[2].Value
                           f m.Groups[3].Value |]
                    else
                        failwith $"Not a colour: {s}"

                let nodes =
                    seq {
                        for n in j.nodes do
                            yield (parseTriple n.id, parseColour n.atoms[0])
                    }

                let maxdim i =
                    let fn (x: array<int> * array<int>) =
                        let coords = fst x
                        coords[i] + 1

                    (Seq.max (Seq.map fn nodes))

                let md =
                    [| for i in 0..2 do
                           maxdim i |]

                let sz = if md[2] <= 1 then 1 else 2

                let img =
                    new SITKUtil.VoxImage(
                        new itk.simple.Image(
                            new itk.simple.VectorUInt32(Array.map uint32 md),
                            itk.simple.PixelIDValueEnum.sitkVectorUInt8,
                            uint32 4
                        )
                    )

                img.GetBufferAsUInt8 (fun buf ->
                    for (coords, colour) in nodes do
                        let linearCoord =
                            coords[0]
                            + (coords[1] * md[0])
                            + (if sz <= 1 then
                                   0
                               else
                                   (coords[2] * md[1] * md[0]))

                        for colourId in [ 0..2 ] do
                            buf.Set (linearCoord + colourId) (uint8 colour[colourId])

                        buf.Set (linearCoord + 3) 255uy)

                img.Save s2         

            | Img, JSon ->
                let (imgf, jsonf) = (s1, s2)
                let img = new SITKUtil.VoxImage(imgf)
                let j = imgToGraph img
                let str = Json.serialize (j)
                use sw = new System.IO.StreamWriter(s2)
                sw.Write str
                sw.Close()
            | Aut, JSon ->
                let graph = autToGraph s1
                let str = Json.serialize graph
                use sw = new System.IO.StreamWriter(s2)
                sw.Write(str)
                sw.Close()
            | _ -> 
                ErrorMsg.Logger.Failure "Invalid conversion arguments, see --help"
                exit 1
            ErrorMsg.Logger.Debug "Conversion done."
            exit 0

        let model = SITKModel() :> IModel
        let checker = ModelChecker(model)

        if parsed.Contains Ops then
            Seq.iter (fun (op: Operator) -> printfn "%s" <| op.Show()) checker.OperatorFactory.Operators
            exit 0

        let run filename =
            let interpreter = Interpreter(model, checker)
            interpreter.Batch false interpreter.DefaultLibDir filename

        match parsed.TryGetResult Filename, ErrorMsg.isDebug () with
        | None, false ->
            printfn "%s\n" (cmdLineParser.PrintUsage())
            0
        | Some filename, _ ->
            run filename
            0
        | None, true ->
            run "test.imgql"
            0
    with
    | e ->
        ErrorMsg.Logger.DebugExn e
        ErrorMsg.Logger.Failure "exiting."
        ErrorMsg.Logger.Failure "exiting."
        ErrorMsg.Logger.Failure "exiting."
        1
