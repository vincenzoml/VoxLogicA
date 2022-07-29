module VoxLogicA.Resources

open System.Collections.Concurrent
open System.Threading.Tasks
open System.Linq

type ResourceKey = string

type HashSet<'a>() =
    inherit ConcurrentDictionary<'a,unit>()
    member this.Add x = this[x] <- ()

    member this.Contains x = this.ContainsKey x

    member this.Remove x =
        this.Keys.Remove x

    new (s : seq<'a>) as this =
        new HashSet<'a>() then
            for x in s do
                this.Add x


type ResourceData<'kind>() =
    inherit ConcurrentDictionary<ResourceKey, 'kind>()

    new(reqs: seq<ResourceKey * 'kind>) as this =
        ResourceData() then
            Seq.iter (fun (k,v) -> this[k] <- v ) reqs

    override this.ToString() =
        let strings =
            (seq {
                for k in this.Keys do
                    yield $"{k} -> {this[k]}"
            })

        let sep = ", "
        let sstart = "{"
        let send = "}"
        $"{sstart} {String.concat sep strings} {send}"

and Requirements<'kind> = ResourceData<'kind>
and Resources<'t, 'kind when 'kind: equality>() =
    inherit ResourceData<Resource<'t,'kind>>()

    member this.Respect(requirements : Requirements<'kind>) =
        query {
            for kv in requirements do
            let key,kind = kv.Key,kv.Value
            all (if this.ContainsKey key then this[key].Kind = kind else false)
        }

    override this.ToString() =
        let strings =
            (seq {
                for k in this.Keys do
                    yield $"{k} -> {this[k]}"
            })

        let sep = ", "
        let sstart = "{"
        let send = "}"
        $"{sstart} {String.concat sep strings} {send}"

and Resource<'t, 'kind when 'kind: equality>(value: 't, kind: 'kind) =
    let assignedTo = new HashSet<obj>() // TODO: do this in debug; for release, use just a reference count.

    member __.AssignedTo = assignedTo.Keys :> seq<obj>

    member __.AssignTo o =
        ignore <| assignedTo.Add o

    member __.Reclaim(o) =
        ignore <| assignedTo.Remove o // TODO: do this in debug; for release, use just a reference count.

    member __.Value = value
    member __.Kind: 'kind = kind

    override __.ToString() = $"Resource [{value}: {kind}]"


type Repository<'a, 'b when 'a: equality>() =
    let dict = ConcurrentDictionary<'a, HashSet<'b>>()

    member __.Item key = dict[key]

    member __.Add (key: 'a) (value: 'b) =
        match dict.TryGetValue key with
        | (false, _) -> dict[key] <- new HashSet<'b>(seq { value })
        | (_, s) -> ignore (s.Add value)

    member __.Pop key =
        match dict.TryGetValue key with
        | (false, _) -> None
        | (_, hs) ->
            if hs.Count = 0 then
                None
            else
                let el = Seq.head (hs.Keys :> seq<'b>)
                ignore <| hs.Remove el
                Some el

    member __.Remove key value =
        match dict.TryGetValue key with
        | (false, _) -> ()
        | (_, hs) -> ignore (hs.Remove value)

    member __.Keys = dict.Keys

    member __.Values = failwith "stub" // TODO: must remove duplicates Seq.concat dict.Values

type ResourceManager<'t, 'kind when 'kind: equality>(allocator: 'kind -> option<'t>) =
    let allocated = Repository<'kind, Resource<'t, 'kind>>()
    let free = Repository<'kind, Resource<'t, 'kind>>()

    let satisfiable (requirements: Requirements<'kind>) =
        query {
            for kind in requirements.Values do
                groupBy kind into g
                all (g.Count() < free[g.Key].Count + allocated[g.Key].Count)
        }

    let mutable waiters: list<{| requirements : Requirements<'kind>; tcs: TaskCompletionSource<option<Resources<'t, 'kind>>> |}> =
        []

    member __.Allocate (requirements: Requirements<'kind>) =
        let tmp = Resources<'t, 'kind>()

        let rec processKVs (s: seq<System.Collections.Generic.KeyValuePair<ResourceKey, 'kind>>) =
            if Seq.isEmpty s then
                Some tmp
            else
                let kv = Seq.head s
                let kvs = Seq.tail s
                let key = kv.Key
                let kind = kv.Value

                match free.Pop kind with // TODO: below a threshold, call allocator, and avoid waiting and freeing
                | None ->
                    match allocator kind with
                    | Some value ->
                        let resource = Resource(value,kind)
                        tmp[key] <- resource
                        allocated.Add kind resource
                        processKVs kvs
                    | None ->
                        for resource in tmp.Values do
                            free.Add resource.Kind resource // They are already allocated, doesn't make sense to throw them away
                        None
                | Some resource ->
                    allocated.Add kind resource
                    free.Remove kind resource
                    tmp[key] <- resource
                    processKVs kvs

        let result = processKVs requirements
        assert
            match result with
            | None -> true
            | Some result -> result.Respect requirements

        result

    member this.Wait (requirements: Requirements<'kind>) =
        if satisfiable requirements then
            let tcs = new TaskCompletionSource<_>()
            waiters <- {| requirements = requirements; tcs = tcs |} :: waiters
            task {
                return! tcs.Task
            }
        else
            task { return None }

    member this.Return(resource: Resource<'t, 'kind>) =
        free.Add resource.Kind resource
        allocated.Remove resource.Kind resource

        waiters <-
            List.filter
                (fun waiter ->
                    let resOpt = this.Allocate waiter.requirements

                    if resOpt.IsSome then
                        waiter.tcs.SetResult resOpt
                        false
                    else if satisfiable waiter.requirements then // TODO: if no recompaction happen, satisfiable can't change
                        true
                    else
                        waiter.tcs.SetResult None
                        false)
                waiters



