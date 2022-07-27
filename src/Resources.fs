module VoxLogicA.Resources

open System.Collections.Generic
open System.Threading.Tasks
open System.Linq

type ResourceKey = string

type ResourceData<'kind>() =
    inherit Dictionary<ResourceKey, 'kind>()

    new(reqs: seq<ResourceKey * 'kind>) as this =
        ResourceData() then
        //assert (Seq.length (Seq.groupBy id (Seq.map fst reqs)) = Seq.length reqs)
            Seq.iter this.Add reqs

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
            all (this[key].Kind = kind)
        }

    // let byKey = new Dictionary<ResourceKey, Resource<'t, 'kind>>()
    // // let byType = new Dictionary<'kind, list<Resource<'t, 'kind>>>()

    // member __.ComplyWith(requirements: Requirements<'kind>) =
    //     Seq.forall
    //         id
    //         (seq {
    //             for kv in byKey do
    //                 yield requirements.AsDictionary[kv.Key] = kv.Value.Kind
    //         })

    // member __.Item
    //     with get x = byKey[x]

    // member __.Values = byKey.Values

    // member __.ByKey k =
    //     try
    //         byKey[k]
    //     with
    //     | :? KeyNotFoundException -> ErrorMsg.fail $"Resource not available: {k}"

    //member __.ByType t = byType[t] :> seq<Resource<'t, 'kind>>

    // member this.Assign k (resource: Resource<'t, 'kind>) =
    //     assert not (byKey.ContainsKey k)
    //     byKey[k] <- resource
    //     let t = resource.Kind

    //     byType[t] <- if byType.ContainsKey t then
    //                      resource :: byType[t]
    //                  else
    //                      [ resource ]

    //     resource.AssignTo this

    // member this.Reclaim() = // deliberately not fine-grained
    //     let v = byKey.Values

    //     for res in v do
    //         res.Reclaim(this)

    //     byKey.Clear()
    //     byType.Clear()
    //     v

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

    member __.AssignedTo = assignedTo :> seq<obj>

    member __.AssignTo o =
        assert (not (assignedTo.Contains o))
        ignore <| assignedTo.Add o

    member __.Reclaim(o) =
        // Do not "assert assignedTo.Contains o" because the result of an operation can be one of the resources passed as input or even one of the arguments
        ignore <| assignedTo.Remove o // TODO: do this in debug; for release, use just a reference count.

    member __.Value = value
    member __.Kind: 'kind = kind

    override __.ToString() = $"Resource [{value}: {kind}]"


type Repository<'a, 'b when 'a: equality>() =
    let dict = Dictionary<'a, HashSet<'b>>()

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
                let el = Seq.head (hs :> seq<'b>)
                ignore <| hs.Remove el
                Some el

    member __.Remove key value =
        match dict.TryGetValue key with
        | (false, _) -> ()
        | (_, hs) -> ignore (hs.Remove value)

    member __.Keys = dict.Keys

    member __.Values = failwith "stub" // TODO: must remove duplicates Seq.concat dict.Values


type ResourceManager<'t, 'kind when 'kind: equality>(allocator: 'kind -> Resource<'t, 'kind>) =
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

        let rec processKVs (s: seq<KeyValuePair<ResourceKey, 'kind>>) =
            if Seq.isEmpty s then
                Some tmp
            else
                let kv = Seq.head s
                let kvs = Seq.tail s
                let key = kv.Key
                let kind = kv.Value

                match free.Pop kind with
                | None ->
                    try
                        let resource = allocator kind
                        tmp[key] <- resource
                        allocated.Add kind resource
                        processKVs kvs
                    with
                    | _ ->
                        for resource in tmp.Values do
                            free.Add resource.Kind resource // They are already allocated, doesn't make sense to throw them away
                        None
                | Some res ->
                    processKVs kvs

        processKVs requirements

    member __.Wait (requirements: Requirements<'kind>) =
        if satisfiable requirements then
            let tcs = new TaskCompletionSource<_>()
            waiters <- {| requirements = requirements; tcs = tcs |} :: waiters
            tcs.Task
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



