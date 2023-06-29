module VoxLogicA.Cache

open VoxLogicA.Reducer
open System.IO

type functionName = string

type CacheNode = 
    { name: functionName
      dependencies: Arguments }

    member this.ReadCache(extension : string) =
        let filename = this.name + extension
        let result =
            match extension with 
            | ".txt" -> System.IO.File.ReadLines(filename)
        result

    member this.WriteCache(extension : string, data) =
        let filename = this.name + extension
        match extension with
        | ".txt" -> System.IO.File.WriteAllText(filename, data)

let generateHash text = 
    let getbytes : (string->byte[]) = System.Text.Encoding.UTF8.GetBytes
    use algorithm = new System.Security.Cryptography.SHA512Managed()
    text |> (getbytes >> algorithm.ComputeHash >> System.Convert.ToBase64String)

type OperatorsCache = 
    { nodes: array<CacheNode> }
    
    member this.ToDot() =
        let mutable str = "digraph {"

        for i = 0 to this.nodes.Length - 1 do

            str <-
                str
                + $"{i} [label=\"[{i}] {this.nodes[i].name}\"];\n"

            for dep in this.nodes[i].dependencies do
                str <- str + $"{i} -> {dep};\n"

        str + "\n}"

let fromWorkPlan program = 
    let operations = program.operations
    { nodes = Array.init (Array.length operations) (fun op -> {
        name = generateHash (operations[op].ToString())
        dependencies = operations[op].arguments
    }) }