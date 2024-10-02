module VoxLogicA.ErrorMsg

open System.IO
// TODO: convert into a new type called Logger

<<<<<<< HEAD
let mutable private debugFlag = false
#if DEBUG
do debugFlag <- true
#endif
let isDebug() = true
=======
exception private VLExn of msg: string * stackTrace: List<string * string>
    with
        override this.Message =
            match this.stackTrace with
            | [] -> this.msg
            | _ -> this.msg + List.fold (fun str (id, pos) -> (sprintf "%s\n%s at %s" str id (pos.ToString()))) "" this.stackTrace
>>>>>>> 17c31905644b20952e5086389a070bcf0ce1e558

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
    static let mutable logLevel = new System.Collections.Generic.HashSet<string>()
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

        if logLevel.Count = 0 || logLevel.Contains prefix then
            lock destinations (fun () -> List.iter printer destinations)

    static member SetLogLevel(x : list<string>) =
        logLevel.Clear()
        for ll in x do
            ignore <| logLevel.Add(ll)

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

    static member Debug s = 
        #if DEBUG
        print "dbug" s
        #else
        ()
        #endif
    static member Test l s = 
        #if DEBUG
        print l s
        #else
        ()
        #endif
    static member Warning s = print "warn" s
    static member Failure s = print "fail" s
    static member Info s = print "info" s
    static member Result name value =
        print "user" (sprintf "%s=%A" name value)

    static member DebugExn(exn: exn) =
        Logger.Failure <|
            #if DEBUG
            exn.ToString()
            #else
            exn.Message
            #endif
