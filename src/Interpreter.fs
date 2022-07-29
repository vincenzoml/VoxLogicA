module VoxLogicA.Interpreter

open VoxLogicA.Reducer

open System.Collections.Generic

open System.Threading.Tasks

open VoxLogicA.Resources

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
    let outputTCS = TaskCompletionSource<Result<'t, 'kind>>()

    member __.Id = id
    member val Requirements = operatorImplementation.Requires

    member __.Started = started

    /// returns only the output resource so that its reference count can be increased before passing it to other ComputeUnits
    member this.Start(resources: Resources<'t, 'kind>) =
        assert resources.Respect this.Requirements

        task {
            let! inputs = Task.WhenAll(Seq.toArray arguments)
            let! output = operatorImplementation.Run resources inputs
            outputTCS.SetResult output
        }

    member val Awaiters = new HashSet<obj>()

    /// Method to read the result
    member __.Result =
        task {

            let! output = outputTCS.Task

            do! output.task

            return output.result
        }

// let runTaskGraph (program: WorkPlan) = Array.mapi mkComputeUnit program.tasks

type Scheduler<'t, 'kind when 'kind: equality> =
    member __.X = ""

open System.Threading

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
                    <| Seq.map (fun i -> computeUnits[i].Result) operation.arguments

                let cu =
                    ComputeUnit(id, executionEngine.ImplementationOf operation.operator, arguments, resourceManager)

                computeUnits[id] <- cu

                for argumentId in operation.arguments do
                    ignore
                    <| computeUnits[ argumentId ].Awaiters.Add cu)

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
                        resource.AssignTo cu

                    do! cu.Start resources

                    try
                        let! result = cu.Result

                        for awaiter in cu.Awaiters do
                            result.AssignTo awaiter

                        ()
                    finally

                        for resource in resources.Values do
                            resource.Reclaim(cu)

                            if Seq.length resource.AssignedTo = 0 then // TODO: add a field to check this on a resource
                                resourceManager.Return resource
            }

        task {
            // System.Threading.Thread.CurrentThread.Priority <- System.Threading.ThreadPriority.Highest
            let toRun = Seq.toArray (Seq.filter ((>=) id) computeUnits.Keys)

            for cuId in toRun do
                do! run cuId

            return! computeUnits[id].Result
        }
