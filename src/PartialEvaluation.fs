module VoxLogicA.PartialEvaluation

open VoxLogicA.Parser
open VoxLogicA.Reducer

type Val =
    | Unbound | VBool of bool | VNumber of float | VString of string

type Environment = Map<int, Val>

type PartialEvaluation = 
    {
        program : seq<string>
        env : Environment
    }

let evaluateProgram (workplan: WorkPlan) (numFrames: int) : PartialEvaluation =
    if numFrames < 1 then
        ErrorMsg.fail $"--numframes is {numFrames}: a specification cannot be unrolled over fewer than one frame"

    // How an application is written in the generated program. The diagnostics go
    // through it too, so that what they name can be found in the file at hand.
    let application (args: int list) =
        match args with
        | [] -> ""
        | _ -> "(" + String.concat "," (List.map (fun a -> $"op{a}") args) + ")"

    // The reduction renumbers the operations, so an identifier of this work plan
    // names nothing the reader can look up: the diagnostics quote what an operand
    // is instead of where it sits.
    let operandOf (id: int) = workplan.operations[id].operator
    let argumentsOf (id: int) = workplan.operations[id].arguments

    let mutable environment: Environment = Map.empty

    // The frame each "frame" operation asks for, and the highest frame each video
    // is asked for. A video has to be loaded before the operations that use it,
    // and how many frames it has to provide is not known until every frame index
    // has been computed, so the frame arithmetic gets a sweep of its own.
    let mutable frameOf: Map<int, int> = Map.empty
    let mutable highestFrameOf: Map<int, int> = Map.empty

    for i in 0 .. workplan.operations.Length - 1 do
        match workplan.operations[i].operator with
        | Identifier "load" ->
            match argumentsOf i with
            | [ v ] ->
                let video =
                    match environment.TryFind v with
                    | Some(VString x) -> (x.Split [| '.' |]).[0]
                    | _ -> ErrorMsg.fail $"load is applied to '{operandOf v}', which is not the name of a video"

                environment <- environment.Add(i, VString video)
            | _ -> ErrorMsg.fail "load must take one argument"
        | Identifier "frame" ->
            match argumentsOf i with
            | [ vid; f ] ->
                match environment.TryFind vid with
                | Some(VString _) -> ()
                | _ -> ErrorMsg.fail $"frames are taken from '{operandOf vid}', which is not a video"

                let frame =
                    match environment.TryFind f with
                    | Some(VNumber x) -> int x
                    | _ ->
                        ErrorMsg.fail (
                            $"'{operandOf f}' is used as a frame index, but it is not a frame number. "
                            + "It has to be a frame number, or be built from the frame reference passed to --providecontext"
                        )

                if frame < 0 then
                    ErrorMsg.fail $"the specification refers to frame {frame}: there is no frame before the first one"

                frameOf <- frameOf.Add(i, frame)

                highestFrameOf <-
                    highestFrameOf.Add(vid, max frame (defaultArg (Map.tryFind vid highestFrameOf) 0))

                environment <- environment.Add(i, Unbound)
            | _ -> ErrorMsg.fail "frame must take two arguments"
        | Identifier "inc" ->
            match argumentsOf i with
            | [ a ] ->
                match environment.TryFind a with
                | Some(VNumber x) -> environment <- environment.Add(i, VNumber(x + 1.0))
                | _ ->
                    ErrorMsg.fail (
                        $"'{operandOf a}' is advanced to the next frame, but it is not a frame number. "
                        + "It has to be built from the frame reference passed to --providecontext"
                    )
            | _ -> ErrorMsg.fail "inc must take one argument"
        | Identifier _ -> environment <- environment.Add(i, Unbound)
        | Number x -> environment <- environment.Add(i, VNumber x)
        | Bool x -> environment <- environment.Add(i, VBool x)
        | String x -> environment <- environment.Add(i, VString x)

    let videoOf (id: int) =
        match environment.TryFind id with
        | Some(VString x) -> x
        | _ -> ErrorMsg.fail "Internal error in module PartialEvaluation. Please report."

    let evaluatedProgram = ResizeArray<string>()
    evaluatedProgram.Add("import \"stdlib2.imgql\"\n")

    // The labellings the existentials range over, with the bound each was
    // unrolled to. Two existentials over the same labelling at the same frame
    // make the same check, which is printed once.
    let bounds = ResizeArray<int * int>()

    // The furthest frame a tracked region is asked for, when it lies past the
    // end of the video: the last step persists there, and the run says so.
    let mutable furthestStep = None

    for i in 0 .. workplan.operations.Length - 1 do
        match workplan.operations[i].operator with
        | Identifier "load" ->
            let video = videoOf i

            for j in 0 .. numFrames - 1 do
                evaluatedProgram.Add($"load {video}At{j} = \"frames/{video}_{j}.png\"")

            // A specification may look past the last frame: an until does it at the
            // innermost step of its unrolling, and an until applied to another one
            // does it once more, at any horizon. Past the end the last frame
            // persists, which is the reading the single repeated frame of the
            // earlier pipeline already took; here it is repeated as far as the
            // specification actually reaches.
            let highest = defaultArg (Map.tryFind i highestFrameOf) 0

            if highest > numFrames - 1 then
                let repeated =
                    if highest = numFrames then
                        $"frame {numFrames}"
                    else
                        $"frames {numFrames} to {highest}"

                ErrorMsg.Logger.Warning
                    $"'{video}' has {numFrames} frame(s) and the specification looks as far as frame {highest}: {repeated} repeat the last one"

                evaluatedProgram.Add(
                    $"// {repeated} repeat frame {numFrames - 1}: past the end the last frame persists"
                )

                for j in numFrames..highest do
                    evaluatedProgram.Add($"load {video}At{j} = \"frames/{video}_{numFrames - 1}.png\"")
        | Identifier "frame" ->
            match argumentsOf i with
            | [ vid; _ ] -> evaluatedProgram.Add($"let op{i} = {videoOf vid}At{frameOf[i]}")
            | _ -> ErrorMsg.fail "frame must take two arguments"
        // The frame arithmetic is over: nothing is left of it in the program.
        | Identifier "inc" -> ()
        // A region followed through the frames, unrolled into one step per frame:
        // the step is the one the frame index asks for. Past the end the last
        // frame persists, and so does what is tracked on it: the last step is a
        // union of components of the last frame, which touching it gives back.
        | Identifier "select" ->
            match argumentsOf i with
            | index :: steps when not (List.isEmpty steps) ->
                let frame =
                    match environment.TryFind index with
                    | Some(VNumber x) -> int x
                    | _ ->
                        ErrorMsg.fail (
                            $"'{operandOf index}' selects the step of a tracked region, but it is not a frame number. "
                            + "It has to be built from the frame reference passed to --providecontext"
                        )

                if frame < 0 then
                    ErrorMsg.fail $"the specification refers to frame {frame}: there is no frame before the first one"

                if frame > steps.Length - 1 then
                    furthestStep <- Some(max frame (defaultArg furthestStep 0))

                evaluatedProgram.Add($"let op{i} = op{List.item (min frame (steps.Length - 1)) steps}")
            | _ -> ErrorMsg.fail "select must take a frame index and one step per frame"
        // An existential, unrolled to a bound on the labels of a labelling: the
        // result stands, and the bound becomes a check printed with the goals.
        | Identifier "bounded" ->
            match argumentsOf i with
            | [ labels; k; result ] ->
                let bound =
                    match environment.TryFind k with
                    | Some(VNumber x) -> int x
                    | _ -> ErrorMsg.fail $"'{operandOf k}' bounds the labels of an existential, but it is not a number"

                if not (bounds.Contains((labels, bound))) then
                    bounds.Add((labels, bound))

                evaluatedProgram.Add($"let op{i} = op{result}")
            | _ -> ErrorMsg.fail "bounded must take three arguments"
        | Identifier x -> evaluatedProgram.Add($"let op{i} = " + x + application (argumentsOf i))
        | Number x -> evaluatedProgram.Add($"let op{i} = " + numberToSyntax x)
        | Bool x -> evaluatedProgram.Add($"let op{i} = " + boolToSyntax x)
        | String x -> evaluatedProgram.Add($"let op{i} = " + "\"" + x + "\"")

    match furthestStep with
    | Some frame ->
        ErrorMsg.Logger.Warning
            $"the specification follows a tracked region as far as frame {frame}, past the last one, {numFrames - 1}: there it persists"
    | None -> ()

    for goal in workplan.goals do
        match goal with
        | GoalSave(x, y) -> evaluatedProgram.Add($"save \"{x}.png\" op{y}")
        | GoalPrint(x, y) -> evaluatedProgram.Add($"print \"{x}\" op{y}")

    // The bound is a hypothesis of the run, not a fact about the data: a label
    // past it is a witness the existential missed, and the result would say
    // false where the specification says true. VoxLogicA cannot abort on it,
    // but it can print it, and whatever drives the run can read it and stop.
    if bounds.Count > 0 then
        evaluatedProgram.Add("// the bounds the existentials were unrolled to: false here means witnesses were missed")

        for (labels, bound) in bounds do
            evaluatedProgram.Add($"print \"labels op{labels} within {bound}\" max(op{labels}) .<=. {bound}")

    { program = evaluatedProgram
      env = environment }
