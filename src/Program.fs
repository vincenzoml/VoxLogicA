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

module VoxLogicA.Main
open System.Reflection
open Argu
open itk.simple
open Microsoft.FSharp.NativeInterop
open System.IO


exception CommandLineException 
    with override __.Message = "Invalid arguments. Usage:\nVoxLogicA <FILENAME>"

type LoadFlags = {
    fname : string;
    numCores : int
}

type CmdLine = 
    | Ext of string
    | [<MainCommandAttribute;UniqueAttribute>] Filename of string    
with
    interface Argu.IArgParserTemplate with
        member s.Usage =
            match s with
            | Filename _ -> "BRATS-style directory"
            | Ext _ -> "Extension of the file to slice"

let getSize (slice : Image) =
    let slice = SimpleITK.Cast(slice,PixelIDValueEnum.sitkInt32)
    let mutable count = 0
    let obj = System.Object()
    let niBuffer = slice.GetBufferAsInt32 ()
    use buffer = new NativeArray<int32>(NativePtr.ofNativeInt niBuffer,(int <| slice.GetNumberOfPixels()) * (int <| slice.GetNumberOfComponentsPerPixel()),obj)
    for i = 0 to buffer.Length - 1 do
        if buffer.UGet i > 0 then 
            count <- count + 1
    count        

let slice dirname (flairF : string) (segF : string) =
    let flair = SimpleITK.ReadImage flairF
    let seg = SimpleITK.ReadImage segF    
    let dimsInt = seg.GetSize()
    let dims = dimsInt |> Seq.map int |> Seq.toArray
    let sizes = Array.create dims.[2] 0
    let extractSize = new VectorUInt32(dimsInt)
    extractSize.[2] <- 0u
    let extractIdx = new VectorInt32([|0;0;0|])
    extractIdx.[0] <- 0
    extractIdx.[1] <- 0
    for i = 0 to dims.[2] - 1 do
        extractIdx.[2] <- int32 i
        use slice = SimpleITK.Extract(seg,extractSize,extractIdx)
        sizes.[i] <- getSize slice
    let candidate = Array.findIndex (fun el -> el = Array.max sizes) sizes
    extractIdx.[2] <- candidate
    let result = SimpleITK.Cast(SimpleITK.Extract(flair,extractSize,extractIdx),PixelIDValueEnum.sitkUInt16)
    let resultSeg = SimpleITK.Cast(SimpleITK.Extract(seg,extractSize,extractIdx),PixelIDValueEnum.sitkUInt16)
    let basename = flairF.Substring(0,flairF.LastIndexOf ".nii.gz") + ""
    let fnameUint16png =  basename + "_slice-max-GTV-uint16.png"
    let fnameSegPng = basename + "_seg-uint16.png"
    SimpleITK.WriteImage (result,fnameUint16png)    
    SimpleITK.WriteImage (resultSeg,fnameSegPng)  

[<EntryPoint>]
let main (argv : string array) =
    let name = Assembly.GetEntryAssembly().GetName()
    let version = name.Version 
    let informationalVersion = ((Assembly.GetEntryAssembly().GetCustomAttributes(typeof<AssemblyInformationalVersionAttribute>, false).[0]) :?> AssemblyInformationalVersionAttribute).InformationalVersion
    ErrorMsg.Logger.Debug (sprintf "%s %s" name.Name informationalVersion)
    let model = SITKModel() :> IModel   
    let checker = ModelChecker(model)          
    if version.Revision <> 0 then ErrorMsg.Logger.Warning (sprintf "You are using a PRERELEASE version of %s. The most recent stable release is %d.%d.%d." name.Name version.Major version.Minor version.Build)                        
    try
        let cmdLineParser = ArgumentParser.Create<CmdLine>(programName = name.Name, errorHandler = ProcessExiter())     
        let parsed = cmdLineParser.Parse argv  
           
        let filename = 
            match parsed.TryGetResult Filename with 
            | Some f -> f 
            | None -> "../BRATS"

        let ext = 
            match parsed.TryGetResult Ext with 
            | Some e -> e 
            | None -> "pflair-uint16-alike.nii.gz"

        let d = System.IO.Directory.GetDirectories filename
        for directory in d do
            let files = System.IO.Directory.GetFiles directory
            let flair = Array.tryFind (fun (fname : string) -> fname.EndsWith ext) files
            let seg = Array.tryFind (fun (fname : string) -> fname.EndsWith "seg.nii.gz") files
            match (flair,seg) with
            | (Some flairF,Some segF) -> 
                ErrorMsg.Logger.Debug <| sprintf "slicing in directory %s, files %s and %s" directory flairF segF 
                slice directory flairF segF
            | _ -> ()
        0

    with e ->        
            ErrorMsg.Logger.DebugExn e
            ErrorMsg.Logger.Failure "exiting."
            1
