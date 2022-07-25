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

and Resource<'t,'kind when 'kind : equality>(value: 't, resourceType : 'kind) =
    let mutable assignedTo = new HashSet<Resources<'t,'kind>>() // TODO: do this in debug; for release, use just a reference count.
    member val ResourceType: 'kind = resourceType
    member __.AssignedTo = assignedTo

    member __.AssignTo resources =
        ignore <| assignedTo.Add resources

    member __.Reclaim(res) =
        assert assignedTo.Contains res
        ignore <| assignedTo.Remove res // TODO: do this in debug; for release, use just a reference count.

    member __.Value = value

and Resources<'t,'kind when 'kind : equality>() =
    let byKey = new Dictionary<ResourceKey, Resource<'t,'kind>>()
    let byType = new Dictionary<'kind, list<Resource<'t,'kind>>>()

    member __.ComplyWith(requirements: Requirements<'kind>) =
        Seq.forall
            id
            (seq {
                for kv in byKey do
                    yield requirements.AsDictionary[kv.Key] = kv.Value.ResourceType
            })

    member __.ByKey k =
        try
            byKey[k]
        with :? KeyNotFoundException ->
            ErrorMsg.fail $"Resource not available: {k}"

    member __.ByType t = byType[t] :> seq<Resource<'t,'kind>>
    member __.Item with get x = byKey[x]

    member this.Assign k (resource: Resource<'t,'kind>) =
        assert not (byKey.ContainsKey k)
        byKey[k] <- resource
        let t = resource.ResourceType
        byType[t] <- if byType.ContainsKey t then resource :: byType[t] else [resource]
        resource.AssignTo this

    member this.Reclaim() = // deliberately not fine-grained
        let v = byKey.Values

        for res in v do
            res.Reclaim(this)

        byKey.Clear()
        byType.Clear()
        v

    override __.ToString() =
        byKey.ToString()

open System.Threading.Tasks

type IResourceManager<'t,'kind when 'kind : equality> =
    abstract member Allocator : Requirements<'kind> -> Task<Resources<'t,'kind>>
