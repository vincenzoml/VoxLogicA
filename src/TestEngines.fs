module VoxLogicA.TestEngines

open VoxLogicA.Interpreter
open VoxLogicA.Resources
open System.Threading.Tasks
open VoxLogicA.Reducer

let rec fib x =
    if x < 2 then
        1
    else
        fib (x - 1) + fib (x - 2)

type ArithmeticsResource() =
    static let mutable id = 0
    let myId = id

    do
        ErrorMsg.Logger.Debug $"Resource #{id} created"
        id <- id + 1

    member val Contents: int = 0 with get, set

    override this.ToString() = $"#{myId} = {this.Contents}"

type ArithmeticsResourceKind = KIntCell
type ArithmeticsResourceManager() =

    interface Resources.IResourceManager<ArithmeticsResource, ArithmeticsResourceKind> with
        member __.Allocator(requirements) =
            task {
                let result = Resources()

                Seq.iter (fun key -> result.Assign key (Resource(ArithmeticsResource(),requirements.AsDictionary[key]))) requirements.AsDictionary.Keys

                return result
            }

type Arithmetics() =
    let opFib =
        OperatorImplementation(
            Requirements([ ("internalAndResult", KIntCell) ]),
            (fun resources args ->
                let task =
                    new Task<_>(
                        (fun () ->
                            let id = (args[1] : Resource<ArithmeticsResource,ArithmeticsResourceKind>).Value.Contents
                            let argsStr = String.concat ", " (Seq.map string args)
                            ErrorMsg.Logger.Debug $"[{id}]: running fib({argsStr}) on resources {resources}"
                            let inp = (args[0].Value: ArithmeticsResource)
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

    interface ExecutionEngine<ArithmeticsResource, ArithmeticsResourceKind> with
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
