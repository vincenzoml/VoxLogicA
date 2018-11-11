// Copyright 2018 Vincenzo Ciancia.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
//
// A copy of the license is available in the file "Apache_License.txt".
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

namespace VoxLogicA

open VoxLogicA
open Parser
open Hopac 
open System.Collections.Generic
open System.IO

type StackTrace = 
    | StackTrace of list<string * FParsec.Position>
    override this.ToString() =
        match this with
        | StackTrace trace -> List.fold (fun str (id,pos) -> (sprintf "%s\n%s at %s" str id (pos.ToString()))) "" trace

exception InterpreterException of st: StackTrace * e : exn 
    with override this.Message = this.e.Message + (string this.st)

exception UnknownIdentifierException of ide : string
    with override this.Message = sprintf "Unknown identifier %s" this.ide

exception CantSaveException of t : Type * s : string
    with override this.Message = sprintf "Saving expression of type %s to file \"%s\" is not supported" (this.t.ToString()) this.s

exception CantPrintException of t : Type
    with override this.Message = sprintf "Printing expression of type %s is not supported" (this.t.ToString())


exception MalformedPathException of prefix : string * file : string * result : string * confined : string
    with override this.Message = sprintf "Malformed path: prefix %s, filename %s, result %s, cannot be confined to %s" 
                                            this.prefix this.file this.result this.confined

type private DVal = Form of Formula | Fun of string list * Expression * Env

and private Env = Map<string,DVal>

type Interpreter(model : IModel, checker : ModelChecker, logger : ErrorMsg.Logger) =
    let defaultLibDir : string = 
        System.IO.Path.GetDirectoryName (System.Reflection.Assembly.GetExecutingAssembly().Location)
    
    let emptyEnv () = (new Map<_,_>([]) : Env)

    let bindList (env : Env) (formals : string list) actuals =        
            List.fold2 (fun (env1 : Env) formal actual -> env1.Add(formal,actual)) env formals actuals

    let rec translateExpression stack (model : #IModel) (formFactory : FormulaFactory) (opFactory : OperatorFactory) (env : Env) expression = 
            match expression with        
            | Float f -> formFactory.CreateConst (f,TNumber)
            | String s -> formFactory.CreateConst (s,TString)
            | Call (pos,ide,args) ->
                if env.ContainsKey ide then
                    match env.[ide] with
                        | Form f -> 
                            if args.Length = 0 then f 
                            else raise (InterpreterException (StackTrace ((ide,pos)::stack),WrongNumberOfArgumentsException(ide,0,args.Length)))
                        | Fun (fargs,body,declenv) -> 
                            let actargs = List.map (fun e -> Form (translateExpression stack model formFactory opFactory env e)) args                        
                            let exenv = 
                                try bindList declenv fargs actargs
                                with _ -> raise (InterpreterException (StackTrace ((ide,pos)::stack),WrongNumberOfArgumentsException(ide,fargs.Length,actargs.Length)))                
                            translateExpression ((ide,pos)::stack) model formFactory opFactory exenv body
                else 
                    let args' = Array.ofList (List.map (translateExpression stack model formFactory opFactory env) args)
                    try
                    let operator = opFactory.[ide]
                    formFactory.Create operator args'
                    with 
                        | :? KeyNotFoundException -> 
                            raise (InterpreterException (StackTrace ((ide,pos)::stack),UnknownIdentifierException(ide))) // TODO: use interpreterexception
                        | :? WrongNumberOfArgumentsException as e ->
                            raise (InterpreterException (StackTrace ((ide,pos)::stack),e))
                        | :? TypeErrorException as e -> 
                            raise (InterpreterException (StackTrace ((ide,pos)::stack),e))
                        | e -> raise e                           


    let getPath (dir : string) (file : string) (confineto : string) = 
        // TODO: before publishing this as a web application, this placeholder function should be improved, and assessed for security
        let dir2 = System.IO.Path.GetFullPath dir
        let file2 = file.TrimStart('/').TrimStart('\\')
        let concat = dir2 + "/" + file2 // NOTE: do not replace this with Path.Combine, since the latter will permit file to be absolute, which would be entirely fine given the check below, but would permit one to check the absolute path of "confineto": if there is no error when requiring "/blah/filename", then "confineto" is a prefix of "blah".
        let res = System.IO.Path.GetFullPath concat // Canonicizes the file name
        let confineto = System.IO.Path.GetFullPath confineto
        if res.StartsWith confineto then res else raise (MalformedPathException (dir,file,concat,confineto))
    let interpreterJob libdir inputdir outputdir confinetoRead confinetoWrite filename (model : #IModel) (checker : ModelChecker) (s : System.IO.Stream) =
        let rec evaluate (env : Env) (parsedImports : Set<string>) syn jobs =        
            match syn with 
                | ModelLoad(ide,filename) :: rest ->
                    let filename = getPath inputdir filename confinetoRead 
                    logger.DebugOnly <| sprintf "ModelLoad \"%s\"" filename
                    let v = model.Load logger filename
                    evaluate (env.Add(ide,Form (checker.FormulaFactory.CreateConst (v,TModel)))) parsedImports rest jobs                
                | Declaration (ide,fargs,body) :: rest -> 
                    logger.DebugOnly <| sprintf "Declaration \"%s\"" ide 
                    evaluate (env.Add(ide,Fun (fargs,body,env))) parsedImports rest jobs
                | ModelSave(pos,filename,expression) :: rest -> // TODO Use interpreterexception also in load and import
                    logger.DebugOnly <| sprintf "ModelSave \"%s\"" filename
                    let formula = translateExpression [] model checker.FormulaFactory checker.OperatorFactory env expression
                    let typ = formula.Operator.Rettype
                    if model.CanSave typ filename then
                        let j = job {   
                            let! res = checker.Get formula
                            let filename = getPath outputdir filename confinetoWrite
                            let dirname = System.IO.Path.GetDirectoryName filename
                            ignore <| Directory.CreateDirectory(dirname)
                            model.Save logger filename res  }
                        evaluate env parsedImports rest (j::jobs)
                    else raise <| InterpreterException(StackTrace(["save",pos]),CantSaveException(typ,filename))                            
                | Print(pos,filename,expression) :: rest -> // TODO Use interpreterexception also in load and import
                    logger.DebugOnly <| sprintf "Print \"%s\"" filename
                    let formula = translateExpression [] model checker.FormulaFactory checker.OperatorFactory env expression
                    let typ = formula.Operator.Rettype
                    match typ with 
                    | (TModel | TNumber | TBool | TString) ->
                            let j = 
                                job {   let! res = checker.Get formula                                
                                        logger.Result filename res  }
                            evaluate env parsedImports rest (j::jobs)
                    | _ -> raise <| InterpreterException(StackTrace(["print",pos]),CantPrintException(typ))                            
                | Import fname :: rest ->
                    let path = 
                        let try1 = getPath "." fname confinetoRead // TODO: also permit local import                        
                        if File.Exists try1 
                        then try1
                        else getPath libdir fname confinetoRead
                    logger.DebugOnly <| sprintf "Import \"%s\"" fname
                    logger.Debug <| sprintf "Importing file \"%s\"" path                    
                    if not (parsedImports.Contains(path)) then 
                        let parsed = parseImport path                    
                        evaluate env (parsedImports.Add path) (parsed@rest) jobs
                    else evaluate env parsedImports rest jobs
                | [] -> List.rev jobs
        job {   logger.Debug "Parsing input..."        
                let p = parseProgram filename s
                logger.Debug "Preparing computation..."                
                let jobs = evaluate (emptyEnv()) (Set.empty) p []
                logger.Debug "Starting computation..."
                do! checker.Check
                do! Job.conIgnore jobs //Util.Concurrent.doParallel (Array.ofList jobs)                  
                logger.Debug "... done."  }
    let batchHopac job = 
        match Hopac.run (Job.catch job) with
            | Choice1Of2 () -> ()
            | Choice2Of2 e -> raise e
    member __.DefaultLibDir = defaultLibDir        

    member __.Batch libdir inputdir outputdir filename =
        let s = new System.IO.FileStream(filename,System.IO.FileMode.Open)
        batchHopac <| interpreterJob libdir inputdir outputdir "/" "/" filename model checker s
        