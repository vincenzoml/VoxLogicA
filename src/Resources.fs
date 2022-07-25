module VoxLogicA.Resources

open System.Collections.Generic

type ResourceKey = string


type Requirements<'kind>(dict: IReadOnlyDictionary<ResourceKey, 'kind>) =
    member __.AsDictionary = dict

    new(reqs: seq<ResourceKey * 'kind>) =
        assert (Seq.length (Seq.groupBy id (Seq.map fst reqs)) = Seq.length reqs)
        let dict = new Dictionary<_, _>()

        Seq.iter dict.Add reqs

        Requirements(dict)

    override __.ToString() =
        let strings =
            (seq {
                for k in dict.Keys do
                    yield $"{k} -> {dict[k]}"
            })

        let sep = ", "
        let sstart = "{"
        let send = "}"
        $"{sstart} {String.concat sep strings} {send}"

and Resource<'t, 'kind when 'kind: equality>(value: 't, kind: 'kind) =
    let mutable assignedTo = new HashSet<Resources<'t, 'kind>>() // TODO: do this in debug; for release, use just a reference count.

    member __.AssignedTo = assignedTo

    member __.AssignTo resources = ignore <| assignedTo.Add resources

    member __.Reclaim(res) =
        assert assignedTo.Contains res
        ignore <| assignedTo.Remove res // TODO: do this in debug; for release, use just a reference count.

    member __.Value = value
    member __.Kind: 'kind = kind

    override __.ToString() = $"Resource [{value}: {kind}]"

and Resources<'t, 'kind when 'kind: equality>() =
    let byKey = new Dictionary<ResourceKey, Resource<'t, 'kind>>()
    let byType = new Dictionary<'kind, list<Resource<'t, 'kind>>>()

    member __.ComplyWith(requirements: Requirements<'kind>) =
        Seq.forall
            id
            (seq {
                for kv in byKey do
                    yield requirements.AsDictionary[kv.Key] = kv.Value.Kind
            })

    member __.ByKey k =
        try
            byKey[k]
        with
        | :? KeyNotFoundException -> ErrorMsg.fail $"Resource not available: {k}"

    member __.ByType t = byType[t] :> seq<Resource<'t, 'kind>>

    member __.Item
        with get x = byKey[x]

    member __.Values = byKey.Values

    member this.Assign k (resource: Resource<'t, 'kind>) =
        assert not (byKey.ContainsKey k)
        byKey[k] <- resource
        let t = resource.Kind

        byType[t] <- if byType.ContainsKey t then
                         resource :: byType[t]
                     else
                         [ resource ]

        resource.AssignTo this

    member this.Reclaim() = // deliberately not fine-grained
        let v = byKey.Values

        for res in v do
            res.Reclaim(this)

        byKey.Clear()
        byType.Clear()
        v

    override __.ToString() =
        let strings =
            (seq {
                for k in byKey.Keys do
                    yield $"{k} -> {byKey[k]}"
            })

        let sep = ", "
        let sstart = "{"
        let send = "}"
        $"{sstart} {String.concat sep strings} {send}"

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

open System.Threading.Tasks

// type IResourceManager<'t, 'kind when 'kind: equality> =
//     abstract member Allocate: Requirements<'kind> -> Task<option<Resources<'t, 'kind>>>
//     abstract member Wait: Requirements<'kind> -> Task<option<Resources<'t, 'kind>>>
//     abstract member Return: Resources<'t, 'kind> -> Task
//     abstract member CanWait: Requirements<'kind> -> bool

open System.Linq

type ResourceManager<'t, 'kind when 'kind: equality>(allocator: 'kind -> Resource<'t, 'kind>) =
    let allocated = Repository<'kind, Resource<'t, 'kind>>()
    let free = Repository<'kind, Resource<'t, 'kind>>()

    // let available (requirements: Requirements<'kind>) =
    //     query {
    //         for kind in requirements.AsDictionary.Values do
    //             groupBy kind into g
    //             all (g.Count() < free[g.Key].Count)
    //     }

    let satisfiable (requirements: Requirements<'kind>) =
        query {
            for kind in requirements.AsDictionary.Values do
                groupBy kind into g
                all (g.Count() < free[g.Key].Count + allocated[g.Key].Count)
        }

    let mutable waiters: list<Requirements<'kind> * TaskCompletionSource<option<Resources<'t, 'kind>>>> =
        []

    member __.Allocate(requirements: Requirements<'kind>) =
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
                        tmp.Assign key resource
                        allocated.Add kind resource
                        processKVs kvs
                    with
                    | _ ->
                        for resource in tmp.Reclaim() do
                            free.Add resource.Kind resource // They are already allocated, doesn't make sense to throw them away

                        None
                | Some res ->
                    tmp.Assign key res
                    processKVs kvs

        processKVs requirements.AsDictionary

    member __.Wait(requirements: Requirements<'kind>) =
        if satisfiable requirements then
            let tcs = new TaskCompletionSource<_>()
            waiters <- (requirements, tcs) :: waiters
            tcs.Task
        else
            task { return None }

    member this.Return(resources: Resources<'t, 'kind>) =
        for resource in resources.Reclaim() do
            free.Add resource.Kind resource
            allocated.Remove resource.Kind resource

        waiters <-
            List.filter
                (fun (requirements: Requirements<'kind>, tcs: TaskCompletionSource<option<Resources<'t, 'kind>>>) ->
                    let resOpt = this.Allocate requirements

                    if resOpt.IsSome then
                        tcs.SetResult resOpt
                        false
                    else if satisfiable requirements then // TODO: if no recompaction happen, satisfiable can't change
                        true
                    else
                        tcs.SetResult None
                        false)
                waiters
