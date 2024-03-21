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

exception CantPrintException of t : Type
    with override this.Message = sprintf "Printing expression of type %s is not supported" (this.t.ToString())


exception MalformedPathException of prefix : string * file : string * result : string * confined : string
    with override this.Message = sprintf "Malformed path: prefix %s, filename %s, result %s, cannot be confined to %s" 
                                            this.prefix this.file this.result this.confined

exception ImportNotFoundException of fname : string * libdir : string 
    with override this.Message = sprintf "Import \"%s\" not found\n(local paths are searched in current director and \"%s\")" this.fname this.libdir         

// exception ImpossibleToDisableGCException of size : int64 
//     with override this.Message = sprintf "Error when disabling garbage collection: impossible to allocate %A bytes" this.size                           

type private DVal = Form of Formula | Fun of string list * Expression * Env

and private Env = Map<string,DVal>

type Interpreter(model : IModel, checker : ModelChecker) =
    let defaultLibDir : string = 
        System.IO.Path.GetDirectoryName(System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName)    
        
    let emptyEnv () = (new Map<_,_>([]) : Env)

    let bindList (env : Env) (formals : string list) actuals =        
            List.fold2 (fun (env1 : Env) formal actual -> env1.Add(formal,actual)) env formals actuals

    let rec translateExpression stack (model : #IModel) (formFactory : FormulaFactory) (opFactory : OperatorFactory) (env : Env) expression = 
            match expression with        
            | Float f -> 
                // // ErrorMsg.Logger.DebugOnly (sprintf "Translating float(%A)" f)
                formFactory.CreateConst (f,TNumber)
            | Bool b ->
                formFactory.CreateConst (b,TBool)
            | String s -> 
                // ErrorMsg.Logger.DebugOnly (sprintf "Translating string(%A)" s)
                formFactory.CreateConst (s,TString)
            | Call (pos,ide,args) ->
                // ErrorMsg.Logger.DebugOnly (sprintf "Translating call(%s)" ide)
                if env.ContainsKey ide then
                    match env.[ide] with
                        | Form f -> 
                            // ErrorMsg.Logger.DebugOnly (sprintf "%s is a formula: %s %d %A" ide f.Operator.Name f.Uid (Array.map (fun (a : Formula) -> a.Operator.Name) f.Arguments))
                            if args.Length = 0 then f 
                            else raise (InterpreterException (StackTrace ((ide,pos)::stack),WrongNumberOfArgumentsException(ide,0,args.Length)))
                        | Fun (fargs,body,declenv) -> 
                            // ErrorMsg.Logger.DebugOnly (sprintf "%s is a function" ide)                            
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

    let interpreterJob libdir filename (model : #IModel) (checker : ModelChecker) (s : System.IO.Stream) =
        let rec evaluate (env : Env) (parsedImports : Set<string>) syn jobs =        
            match syn with 
                | ModelLoad(ide,filename) :: rest ->
                    let filename = System.IO.Path.GetFullPath filename 
                    // ErrorMsg.Logger.DebugOnly <| sprintf "Interpreter: ModelLoad \"%s\"" filename
                    let v = model.Load filename
                    let fmla = checker.FormulaFactory.CreateConst (v,TModel)
                    // ErrorMsg.Logger.DebugOnly (sprintf "Interpreter: loaded image %A name %s from file %s fmla uid %d" (v.GetHashCode()) ide filename fmla.Uid)
                    let env = env.Add(ide,Form fmla)
                    evaluate env parsedImports rest jobs                
                | Declaration (ide,fargs,body) :: rest -> 
                    // ErrorMsg.Logger.DebugOnly <| sprintf "Interpreter: Declaration \"%s\"" ide 
                    evaluate (env.Add(ide,Fun (fargs,body,env))) parsedImports rest jobs
                | ModelSave(pos,filename,expression) :: rest -> // TODO Use interpreterexception also in load and import
                    // ErrorMsg.Logger.DebugOnly <| sprintf "Interpreter: ModelSave \"%s\"" filename
                    let formula = translateExpression [] model checker.FormulaFactory checker.OperatorFactory env expression
                    let typ = formula.Operator.Rettype
                    if model.CanSave typ filename then
                        let j = job {   
                            let! res = checker.Get formula
                            // TODO: Changed saving policy with OnExit operation
                            let fullFilename = System.IO.Path.GetFullPath filename
                            let dirname = System.IO.Path.GetDirectoryName fullFilename
                            ignore <| Directory.CreateDirectory(dirname)
                            // ErrorMsg.Logger.DebugOnly (sprintf "Interpreter: About to save image: %A" <| res.GetHashCode())
                            model.Save filename res  }
                        evaluate env parsedImports rest (j::jobs)
                    else raise <| InterpreterException(StackTrace(["save",pos]),CantSaveException(typ,filename))                            
                | Print(pos,filename,expression) :: rest -> // TODO Use interpreterexception also in load and import
                    // ErrorMsg.Logger.DebugOnly <| sprintf "Interpreter: Print \"%s\"" filename
                    let formula = translateExpression [] model checker.FormulaFactory checker.OperatorFactory env expression
                    let typ = formula.Operator.Rettype
                    match typ with 
                    | (TModel | TNumber | TBool | TString) ->
                            let j = 
                                job {   let! res = checker.Get formula                                
                                        ErrorMsg.Logger.Result filename res  }
                            evaluate env parsedImports rest (j::jobs)
                    | _ -> raise <| InterpreterException(StackTrace(["print",pos]),CantPrintException(typ))                            
                | Import fname :: rest ->
                    let find filename = 
                        if File.Exists filename 
                        then Some filename 
                        else 
                            let nfilename = filename + ".imgql" 
                            if File.Exists nfilename 
                            then Some nfilename
                            else None
                    let path =                         
                        let try1 = System.IO.Path.GetFullPath fname // TODO: also permit local import         
                        match find try1 with
                            | Some try1 -> try1
                            | None ->
                                if not (fname.StartsWith "/") then
                                    let try2 = System.IO.Path.GetFullPath (System.IO.Path.Combine(libdir,fname))
                                    match find try2 with
                                        | Some try2 -> try2
                                        | None -> raise <| ImportNotFoundException(fname,libdir)
                                else raise <| ImportNotFoundException(fname,libdir)                            
                    ErrorMsg.Logger.DebugOnly <| sprintf "Import \"%s\"" fname
                    if not (parsedImports.Contains(path)) then 
                        ErrorMsg.Logger.Debug <| sprintf "Importing file \"%s\"" path                                                               
                        let parsed = parseImport path                    
                        evaluate env (parsedImports.Add path) (parsed@rest) jobs
                    else evaluate env parsedImports rest jobs
                | [] -> List.rev jobs
        job {   ErrorMsg.Logger.Debug "Parsing input..."        
                let p = parseProgram filename s
                ErrorMsg.Logger.Debug "Preparing computation..."                
                let jobs = evaluate (emptyEnv()) (Set.empty) (Import "stdlib.imgql"::p) []
                ErrorMsg.Logger.Debug "Starting computation..."                
                do! checker.Check
                do! Util.Concurrent.conIgnore (Array.ofList jobs)                                  
                ErrorMsg.Logger.Debug "... done."  }
    let batchHopac sequential job = 
        let scheduler = Scheduler.create { Scheduler.Create.Def with NumWorkers = if sequential then Some 1 else None  }
        match Scheduler.run scheduler (Job.catch job) with
            | Choice1Of2 () -> ()
            | Choice2Of2 e -> raise e
        // TODO: this is definitely not the intended usage
        Scheduler.wait scheduler
    member __.DefaultLibDir = defaultLibDir

    member __.Batch sequential libdir filename =
        // TODO: check whether using the following code (pre-allocating 9GB of data) improves performance
        // let size = 1024L * 1024L *1024L * 9L
        // if not (System.GC.TryStartNoGCRegion(size)) 
        // then raise (ImpossibleToDisableGCException(size)) // Exception declaration at the beginning of this file
        // try
        let s = new FileStream(filename,FileMode.Open)
        batchHopac sequential <| interpreterJob libdir filename model checker s
        // with e ->        
        //     System.GC.EndNoGCRegion()
        //     raise e
