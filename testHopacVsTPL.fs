open Hopac
open Hopac.Infixes
open System.Threading.Tasks

let rec fib n = 
    if n < 2 then n
    else fib (n - 1) + fib (n - 2)

let rec hfibJ (n, level) = Job.delay <| fun () ->
  if n < 2 then
    Job.result n
  elif n < level then Job.result (fib n)
  else    
    job {
        let! (x,y) = hfibJ (n-2, level) <*> hfibJ (n-1, level) 
        return x + y
    }

let hfib x = Hopac.run (hfibJ x)

let rec afibA (n, level) = async {
  if n < 2 then
    return n
  elif n < level then return fib n
  else
    let! n2a = afibA (n-2, level) |> Async.StartChild
    let! n1 = afibA (n-1, level)
    let! n2 = n2a
    return n2 + n1
}

let afib x = Async.RunSynchronously (afibA x)

let rec tfib (n, level) =
  if n < 2 then n
  elif n < level then fib n
  else
    let n2t = Task.Factory.StartNew (fun _ -> tfib (n-2, level))
    let n1 = tfib (n-1, level)
    n2t.Result + n1

let run label f x =
    printfn "Running %s..." label
    let t0 = System.DateTime.Now
    let r = f x
    let t1 = System.DateTime.Now
    let x = t1 - t0
    printfn "Terminated %s after %d milliseconds" label (int x.TotalMilliseconds) 
    r

let x = int <| System.Environment.GetCommandLineArgs().[1]
let l = int <| System.Environment.GetCommandLineArgs().[2]
ignore <| run "hopac" hfib (x,l)
ignore <| run "tpl" tfib (x,l)
