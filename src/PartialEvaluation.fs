module VoxLogicA.PartialEvaluation

open VoxLogicA.Reducer

type Val =
    | Unbound | VBool of bool | VNumber of float | VString of string

type Environment = Map<int, Val>

type PartialEvaluation = 
    {
        program : seq<string>
        env : Environment
    }

let evaluateProgram (workplan : WorkPlan) (numFrames : int) : PartialEvaluation =
    if numFrames < 1 then
        ErrorMsg.fail $"a specification cannot be unrolled over {numFrames} frames"

    // Frames 0 to numFrames - 1 are the frames of the video; frame numFrames is a
    // repetition of the last one, so that the innermost step of the unrolling of an
    // until has a frame to look at. Nothing may refer past it.
    let lastFrame = numFrames

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

    let mutable environment = Map.empty
    let mutable evaluatedProgram : seq<string> = Seq.empty
    evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ("import \"stdlib2.imgql\"\n"))
    for i in 0 .. workplan.operations.Length - 1 do
        match workplan.operations[i].operator with
        | Identifier "load" ->
            match Seq.toList workplan.operations[i].arguments with
            | [v] -> 
                let video = 
                    match environment.TryFind v with
                    | Some (VString x) -> (x.Split [|'.'|]).[0]
                    | _ -> ErrorMsg.fail $"load is applied to '{operandOf v}', which is not the name of a video"
                environment <- environment.Add(i, VString video)
                for j in 0 .. numFrames - 1 do
                    evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ("load " + video + $"At{j} = " + "\"frames/" + video + $"_{j}.png" + "\""))
                evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ($"// frame {lastFrame} repeats frame {numFrames - 1}: the unrolling can look one step past the last frame"))
                evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ("load " + video + $"At{lastFrame} = " + "\"frames/" + video + $"_{numFrames-1}.png" + "\""))
            | _ -> failwith "load must take one argument"
        | Identifier "frame" ->
            match Seq.toList workplan.operations[i].arguments with
            | [vid;f] -> 
                let video = 
                    match environment.TryFind vid with
                    | Some (VString x) -> x
                    | _ -> ErrorMsg.fail $"frames are taken from '{operandOf vid}', which is not a video"
                let frame = 
                    match environment.TryFind f with
                    | Some (VNumber x) -> int x
                    | _ ->
                        ErrorMsg.fail (
                            $"'{operandOf f}' is used as a frame index, but it is not a frame number. "
                            + "It has to be a frame number, or be built from the frame reference passed to --providecontext"
                        )
                if frame < 0 || frame > lastFrame then
                    // No advice here: raising --numframes helps a chain of next
                    // operators, but not a temporal operator applied to another
                    // one, which overshoots by the same amount at every horizon.
                    ErrorMsg.fail (
                        $"the specification refers to frame {frame} of '{video}', but only frames 0 to {lastFrame} exist: "
                        + $"{numFrames} frame(s), and one repetition of the last one for the step that looks past the end"
                    )
                evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ("let op" + $"{i} = " + video + $"At{frame}"))
                environment <- environment.Add(i, Unbound)
            | _ -> failwith "frame must take two arguments"
        | Identifier "inc" -> 
            match Seq.toList workplan.operations[i].arguments with
            | [a] -> 
                match environment.TryFind a with
                | Some (VNumber x) -> environment <- environment.Add(i, VNumber (x + 1.0))
                | _ ->
                    ErrorMsg.fail (
                        $"'{operandOf a}' is advanced to the next frame, but it is not a frame number. "
                        + "It has to be built from the frame reference passed to --providecontext"
                    )
            | _ -> failwith "inc must take one argument"
        | Identifier x ->
            // An identifier the partial evaluator knows nothing about goes through
            // as it is, together with its arguments, for VoxLogicA to resolve. With
            // no arguments it goes through on its own: "x()" is not a term of the
            // language, and building the argument list by trimming a trailing comma
            // used to turn that case into the unbalanced "let op0 = x)".
            let args = Seq.toList workplan.operations[i].arguments
            environment <- environment.Add(i, Unbound)
            evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ($"let op{i} = " + x + application args))
        | Number x -> 
            environment <- environment.Add(i, VNumber x)
            evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ("let op" + $"{i} = " + x.ToString()))
        | Bool x -> 
            environment <- environment.Add(i, VBool x)
            evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ("let op" + $"{i} = " + x.ToString()))
        | String x -> 
            environment <- environment.Add(i, VString x)
            evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ("let op" + $"{i} = " + "\"" + x + "\""))

    for goal in workplan.goals do
        match goal with
        | GoalSave(x, y) -> 
            evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ("save \"" + x + ".png\" op" + $"{y}"))
        | GoalPrint(x, y) -> evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ("print \"" + x + "\" op" + $"{y}"))
    {
        program = evaluatedProgram
        env = environment
    }