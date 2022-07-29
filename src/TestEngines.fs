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

type FibResource private (myId) =
    static let maxId = 100009
    static let mutable id = 0

    static member Allocator _ =
        if id < maxId then
            ErrorMsg.Logger.Debug $"Resource #{id} created"
            let result = Some (new FibResource(id))
            id <- id + 1
            result
        else
            None

    member val Contents: int = 0 with get, set

    override this.ToString() = $"#{myId} = {this.Contents}"


type Fib() =
    let opFibSeq =
        OperatorImplementation(
            Requirements(
                [ ("internalAndResult", KIntCell)
                  ("unused", KIntCell) ]
            ),
            (fun resources args ->
                let t = task {
                    let id =
                        (args[1]: Resource<FibResource, FibResourceKind>)
                            .Value
                            .Contents

                    let argsStr = String.concat ", " (Seq.map string args)
                    ErrorMsg.Logger.Debug $"[{id}]: running fib({argsStr}) on resources {resources}"
                    let inp = (args[0].Value: FibResource)
                    let resource = resources["internalAndResult"]
                    let result = fib inp.Contents
                    resource.Value.Contents <- result
                    ErrorMsg.Logger.Debug $"[{id}]: finished fib({argsStr}) on resources {resources}"
                }

                let x =
                    { task = t
                      result = resources["internalAndResult"] }

                task { return x })
        )


    let opFibBackground =
        OperatorImplementation(
            Requirements(
                [ ("internalAndResult", KIntCell)
                  ("unused", KIntCell) ]
            ),
            (fun resources args ->
                let t () =
                    let id =
                        (args[1]: Resource<FibResource, FibResourceKind>)
                            .Value
                            .Contents

                    let argsStr = String.concat ", " (Seq.map string args)
                    ErrorMsg.Logger.Debug $"[{id}]: running fib({argsStr}) on resources {resources}"
                    let inp = (args[0].Value: FibResource)
                    let resource = resources["internalAndResult"]
                    let result = fib inp.Contents
                    resource.Value.Contents <- result
                    ErrorMsg.Logger.Debug $"[{id}]: finished fib({argsStr}) on resources {resources}"

                let x =
                    { task =
                        Task.Run t
                      result = resources["internalAndResult"] }

                task { return x })
        )


    let opFib =
        OperatorImplementation(
            Requirements(
                [ ("internalAndResult", KIntCell)
                  ("unused", KIntCell) ]
            ),
            (fun resources args ->
                let t =
                    new Task<_>(
                        (fun () ->
                            let id =
                                (args[1]: Resource<FibResource, FibResourceKind>)
                                    .Value
                                    .Contents

                            let argsStr = String.concat ", " (Seq.map string args)
                            ErrorMsg.Logger.Debug $"[{id}]: running fib({argsStr}) on resources {resources}"
                            let inp = (args[0].Value: FibResource)
                            let resource = resources["internalAndResult"]
                            let result = fib inp.Contents // Could use resource if needed
                            resource.Value.Contents <- result
                            ErrorMsg.Logger.Debug $"[{id}]: finished fib({argsStr}) on resources {resources}"), // NOTE: can return the same resource; if the result must be a different resource it must be added to the requirements with a specific key
                        TaskCreationOptions.PreferFairness
                    )

                t.Start()

                let x =
                    { task = t
                      result = resources["internalAndResult"] }

                task { return x })
        )

    let opDelay =
        OperatorImplementation(
            Requirements(
                [ ("internalAndResult", KIntCell)
                  ("unused", KIntCell) ]
            ),
            (fun resources args ->
                let t = task {
                    let id =
                        (args[1]: Resource<FibResource, FibResourceKind>)
                            .Value
                            .Contents

                    let argsStr = String.concat ", " (Seq.map string args)
                    ErrorMsg.Logger.Debug $"[{id}]: running delay({argsStr}) on resources {resources}"
                    let inp = (args[0].Value: FibResource)
                    let resource = resources["internalAndResult"]
                    do! Task.Delay inp.Contents
                    resource.Value.Contents <- inp.Contents
                    ErrorMsg.Logger.Debug $"[{id}]: finished delay({argsStr}) on resources {resources}"
                }

                let x =
                    { task = t
                      result = resources["internalAndResult"] }

                task { return x })
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

                            return
                                { result = resource
                                  task = Task.CompletedTask }
                        })
                )
            | Identifier "fib" -> opFib
            | Identifier "fibSeq" -> opFibSeq
            | Identifier "fibBackground" -> opFibBackground
            | Identifier "delay" -> opDelay
            | Identifier "succ" ->
                OperatorImplementation(
                    Requirements(["result",KIntCell]),
                    (fun resources args ->
                        task {
                            let resource = resources["result"]
                            resource.Value.Contents <- args[0].Value.Contents + 1

                            return
                                { result = resource
                                  task = Task.CompletedTask }
                        })
                )
            | _ -> ErrorMsg.fail $"Unknown operator: {s}"
