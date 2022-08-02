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
    static let maxResources = 10
    static let mutable id = 0

    static member Allocator _ =
        if id < maxResources then
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
                [ ("result", KIntCell)
                  ("unused", KIntCell) ]
            ),
            (fun resources args ->
                task {
                    let id =
                        (args[1]: Resource<FibResource, FibResourceKind>)
                            .Value
                            .Contents

                    let argsStr = String.concat ", " (Seq.map string args)
                    ErrorMsg.Logger.Debug $"[{id}]: running fib({argsStr}) on resources {resources}"
                    let inp = (args[0].Value: FibResource)
                    let resource = resources["result"]
                    let result = fib inp.Contents
                    resource.Value.Contents <- result
                    ErrorMsg.Logger.Debug $"[{id}]: finished fib({argsStr}) on resources {resources}"
                    return resources["result"]
                })
        )

    let opFib =
        OperatorImplementation(
            Requirements(
                [ ("result", KIntCell)
                  ("unused", KIntCell) ]
            ),
            (fun resources args ->
                let t () =
                    let id =
                        (args[1]: Resource<FibResource, FibResourceKind>)
                            .Value
                            .Contents

                    let argsStr = String.concat ", " (Seq.map string args)
                    let inp = (args[0].Value: FibResource)
                    let resource = resources["result"]
                    let result = fib inp.Contents
                    resource.Value.Contents <- result
                    resource
                
                Task.Run t)
        )

    let opDelay =
        OperatorImplementation(
            Requirements(
                [ ("result", KIntCell)
                  ("unused", KIntCell) ]
            ),
            (fun resources args ->
                task {
                    let id =
                        (args[1]: Resource<FibResource, FibResourceKind>)
                            .Value
                            .Contents

                    let inp = (args[0].Value: FibResource)
                    let resource = resources["result"]
                    do! Task.Delay inp.Contents
                    resource.Value.Contents <- inp.Contents
                    return resource
                })

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
            | Identifier "fibSeq" -> opFibSeq
            | Identifier "fib" -> opFib
            | Identifier "delay" -> opDelay
            | Identifier "succ" ->
                OperatorImplementation(
                    Requirements(["result",KIntCell]),
                    (fun resources args ->
                        task {
                            let resource = resources["result"]
                            resource.Value.Contents <- args[0].Value.Contents + 1
                            return resource
                        })
                )
            | _ -> ErrorMsg.fail $"Unknown operator: {s}"
