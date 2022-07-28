module VoxLogicA.ErrorMsg

open System.IO
// TODO: convert into a new type called Logger

exception private VLExn of msg: string * stackTrace: List<string * string>
    with
        override this.Message =
            match this.stackTrace with
            | [] -> this.msg
            | _ -> this.msg + List.fold (fun str (id, pos) -> (sprintf "%s\n%s at %s" str id (pos.ToString()))) "" this.stackTrace

        override this.ToString() =
            this.Message

type Stack = list<string * string>

let fail msg = raise (VLExn (msg,[]))
let failWithStacktrace msg (stackTrace : Stack) = raise (VLExn (msg,stackTrace))

type Report private () =
    static let mutable print = []
    static let mutable save = []

    static member Print (name : string,typ : string, res : string) = lock print (fun () -> print <- (name,typ,res)::print)
    static member Save (name : string,typ : string, min : float, max : float, path :string) = lock save (fun () -> save <- (name,typ,min,max,path)::save)
    static member Get () = (List.rev print,List.rev save)

type Logger private () =
    static let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    static let mutable destinations = [] // this is a placeholder for permitting more simultaneous destinations.

    static let print prefix (string: string) =
        let printer (destination : TextWriter) =
            lock destination (fun () ->
                fprintfn
                    destination
                    "[%10dms] [%s] %s"
                    stopWatch.ElapsedMilliseconds
                    prefix
                    (string.Replace("\n", "\n                      "))
                destination.Flush())

        lock destinations (fun () -> List.iter printer destinations)

    static member LogToStdout() =
        lock destinations (fun () ->
            let str = System.Console.OpenStandardOutput()
            let sw = new StreamWriter(str)
            destinations <- sw :: destinations)

    static member LogToMemory() =
        lock destinations (fun () ->
            let str = new MemoryStream(1000000)
            let sw = new StreamWriter(str)
            let sr = new StreamReader(str)
            destinations <- sw :: destinations
            fun () ->
                sw.Flush()
                ignore <| str.Seek(0L,SeekOrigin.Begin)
                sr.ReadToEnd())

    static member Debug s = print "info" s
    static member Assert s = print "debg" s; true 
    static member Warning s = print "warn" s
    static member Failure s = print "fail" s

    static member Result name value =
        print "user" (sprintf "%s=%A" name value)

    static member DebugExn(exn: exn) =
        Logger.Failure <|
            #if DEBUG
            exn.ToString()
            #else
            exn.Message
            #endif
