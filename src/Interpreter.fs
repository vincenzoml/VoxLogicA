module VoxLogicA.Interpreter

open VoxLogicA.Reducer


open System.Threading.Tasks

open VoxLogicA.Resources
open System.Collections.Generic

type Result<'t, 'kind when 'kind: equality> =
    { task: Task
      result: Resource<'t, 'kind> }

type OperatorImplementation<'t, 'kind when 'kind: equality>
    (
        requirements,
        run: Resources<'t, 'kind> -> array<Resource<'t, 'kind>> -> Task<Result<'t, 'kind>>
    ) =

    member __.Requires = requirements

    member __.Run (resources: Resources<'t, 'kind>) arguments =
        assert resources.Respect requirements
        run resources arguments

type ExecutionEngine<'t, 'kind when 'kind: equality> =
    abstract member ImplementationOf: Operator -> OperatorImplementation<'t, 'kind>

type ComputeUnit<'t, 'kind when 'kind: equality>
    (
        id: int,
        operatorImplementation: OperatorImplementation<'t, 'kind>,
        arguments: seq<Task<Resource<'t, 'kind>>>,
        resourceManager: ResourceManager<'t, 'kind>
    ) =
    let mutable started = false
    let outputTCS = TaskCompletionSource<seq<Resource<'t, 'kind>> * Result<'t, 'kind>>()

    member __.Id = id
    member val Requirements = operatorImplementation.Requires

    member __.Started = started

    /// returns only the output resource so that its reference count can be increased before passing it to other ComputeUnits
    member this.Start(resources: Resources<'t, 'kind>) =
        assert resources.Respect this.Requirements

        task {
            let! inputs = Task.WhenAll(Seq.toArray arguments)
            let! output = operatorImplementation.Run resources inputs

            outputTCS.SetResult(inputs, output)
        }

    member val Awaiters = new HashSet<obj>()

    /// Method to read the result
    member __.Result =
        task {

            let! output = outputTCS.Task

            do! (snd output).task

            return (fst output), (snd output).result
        }

type Interpreter<'t, 'kind when 'kind: equality>
    (
        executionEngine: ExecutionEngine<'t, 'kind>,
        resourceManager: ResourceManager<'t, 'kind>
    ) =
    let computeUnits = new Dictionary<int, ComputeUnit<'t, 'kind>>()
    let started = new HashSet<int>()


    member __.Prepare(program: WorkPlan) =
        Array.iteri
            (fun id operation ->
                let arguments =
                    Seq.toArray
                    <| Seq.map
                        (fun i ->
                            task {
                                let! _, x = computeUnits[i].Result
                                return x
                            })
                        operation.arguments

                let cu =
                    ComputeUnit(id, executionEngine.ImplementationOf operation.operator, arguments, resourceManager)

                computeUnits[id] <- cu

                for argumentId in operation.arguments do
                    printfn $"ADDING AWAITER {cu.Id} to {argumentId}"
                    ignore                    
                    <| computeUnits[argumentId].Awaiters.Add cu)

            program.operations

    member __.QueryAsync(id: OperationId) =
        assert computeUnits.ContainsKey id

        let allocate requirements =
            task {

                let resourcesOpt = resourceManager.Allocate(requirements)

                match resourcesOpt with
                | Some resources -> return resources
                | None ->
                    let! resourcesOpt = resourceManager.Wait requirements

                    match resourcesOpt with
                    | None ->
                        return
                            (ErrorMsg.fail
                                $"Not enough resources to satisfy requirements {requirements} for computeUnit #{id}")
                    | Some resources -> return resources
            }

        let run (cuId: int) =
            task {
                if not (started.Contains cuId) then
                    started.Add cuId
                    assert ErrorMsg.Logger.Assert $"starting cuId {cuId}"
                    let cu = computeUnits[cuId]
                    let! resources = allocate cu.Requirements

                    for resource in resources.Values do
                        printfn $"ASSIGNING RESOURCE {resource} to {cuId}"
                        resource.AssignTo cu

                    do! cu.Start resources

                    let! (arguments,result) = cu.Result

                    for awaiter in cu.Awaiters do
                        printfn $"ASSIGNING RESOURCE {result} to awaiter {(awaiter :?> ComputeUnit<'t,'kind>).Id} of {cuId}"
                        result.AssignTo awaiter

                    let deallocate = Seq.toArray <| Seq.concat [ resources.Values :> seq<Resource<'t,'kind>>; arguments ]
                    let odd = 
                        Array.filter 
                            (fun (res : Resource<'t,'kind>) -> not (Seq.contains (cu :> obj) res.AssignedTo))
                            deallocate

                    for resource in deallocate do
                        printfn $"REVOKING RESOURCE {resource} from {cuId}"
                        resource.Reclaim cu

                        if Seq.length resource.AssignedTo = 0 then // TODO: add a field to check this on a resource
                            printfn "RELEASING RESOURCE {resource}"
                            resourceManager.Return resource
            }

        task {
            // System.Threading.Thread.CurrentThread.Priority <- System.Threading.ThreadPriority.Highest
            let toRun = Seq.toArray (Seq.filter ((>=) id) computeUnits.Keys)

            for cuId in toRun do
                do! run cuId

            return! computeUnits[id].Result
        }
