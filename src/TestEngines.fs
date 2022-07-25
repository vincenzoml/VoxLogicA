module VoxLogicA.TestEngines

open VoxLogicA.Interpreter
open VoxLogicA.Resources
open System.Threading.Tasks
open VoxLogicA.Reducer
open System.Collections.Generic

let rec fib x =
    if x < 2 then
        1
    else
        fib (x - 1) + fib (x - 2)


type FibResourceKind = KIntCell
type FibResource private () =
    let max = 10
    static let mutable id = 0
    let myId = id

    do
        if id < max then
            ErrorMsg.Logger.Debug $"Resource #{id} created"
            id <- id + 1
        else
            failwith ""

    static member NewResourceManager () = new Resources.ResourceManager<_,_>(fun _ -> Resources.Resource(FibResource(),KIntCell))

    member val Contents: int = 0 with get, set

    override this.ToString() = $"#{myId} = {this.Contents}"


type Fib() =
    let opFib =
        OperatorImplementation(
            Requirements([ ("internalAndResult", KIntCell); ("unused", KIntCell) ]),
            (fun resources args ->
                let task =
                    new Task<_>(
                        (fun () ->
                            let id = (args[1] : Resource<FibResource,FibResourceKind>).Value.Contents
                            let argsStr = String.concat ", " (Seq.map string args)
                            ErrorMsg.Logger.Debug $"[{id}]: running fib({argsStr}) on resources {resources}"
                            let inp = (args[0].Value: FibResource)
                            let resource = resources.ByKey "internalAndResult" // Same key as in the requirements
                            let result = fib inp.Contents // Could use resource if needed
                            resource.Value.Contents <- result
                            ErrorMsg.Logger.Debug $"[{id}]: finished fib({argsStr}) on resources {resources}"
                            resource), // NOTE: can return the same resource; if the result must be a different resource it must be added to the requirements with a specific key
                        TaskCreationOptions.PreferFairness
                    )

                task.Start()
                task)
        )

    interface ExecutionEngine<FibResource, FibResourceKind> with
        member __.ImplementationOf s =
            match s with
            | Number n ->
                OperatorImplementation(
                    Requirements([ ("result", KIntCell) ]),
                    (fun resources _ ->
                        task {
                            let resource = resources["result"]
                            resource.Value.Contents <- int n
                            return resource
                        })
                )
            | (Identifier "fib"
            | Identifier "+") -> opFib
            | _ -> ErrorMsg.fail $"Unknown operator: {s}"
