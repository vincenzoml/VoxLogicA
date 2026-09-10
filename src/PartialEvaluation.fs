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
                    | _ -> failwith "unbound value"
                environment <- environment.Add(i, VString video)
                for j in 0 .. numFrames - 1 do
                    evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ("load " + video + $"At{j} = " + "\"frames/" + video + $"_{j}.png" + "\""))
                evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ("load " + video + $"At{numFrames} = " + "\"frames/" + video + $"_{numFrames-1}.png" + "\""))
            | _ -> failwith "load must take one argument"
        | Identifier "frame" ->
            match Seq.toList workplan.operations[i].arguments with
            | [vid;f] -> 
                let video = 
                    match environment.TryFind vid with
                    | Some (VString x) -> x
                    | _ -> failwith "unbound value"
                let frame = 
                    match environment.TryFind f with
                    | Some (VNumber x) -> int x
                    | _ -> failwith "unbound value"
                evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ("let op" + $"{i} = " + video + $"At{frame}"))
                environment <- environment.Add(i, Unbound)
            | _ -> failwith "frame must take two arguments"
        | Identifier "inc" -> 
            match Seq.toList workplan.operations[i].arguments with
            | [a] -> 
                match environment.TryFind a with
                | Some (VNumber x) -> environment <- environment.Add(i, VNumber (x + 1.0))
                | _ -> failwith "unbound value"
            | _ -> failwith "inc must take one argument"
        | Identifier x ->
            // An identifier the partial evaluator knows nothing about goes through
            // as it is, together with its arguments, for VoxLogicA to resolve. With
            // no arguments it goes through on its own: "x()" is not a term of the
            // language, and building the argument list by trimming a trailing comma
            // used to turn that case into the unbalanced "let op0 = x)".
            let args = Seq.toList workplan.operations[i].arguments

            let application =
                match args with
                | [] -> ""
                | _ -> "(" + String.concat "," (List.map (fun a -> $"op{a}") args) + ")"

            environment <- environment.Add(i, Unbound)
            evaluatedProgram <- Seq.append evaluatedProgram (Seq.singleton ($"let op{i} = " + x + application))
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