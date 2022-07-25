module VoxLogicA.Resources

open System.Collections.Generic

type ResourceType =
    interface
    end


type ResourceKey = string

type Requirements(dict: IReadOnlyDictionary<ResourceKey, ResourceType>) =
    member __.ByKey x = dict[x]

    static member Null = Requirements(new Dictionary<_, _>())

    new(reqs: seq<ResourceKey * ResourceType>) =
        assert (Seq.length (Seq.groupBy id (Seq.map fst reqs)) = Seq.length reqs)
        let dict = new Dictionary<_, _>()

        Seq.iter dict.Add reqs

        Requirements(dict)

and Resource<'t>(value: 't, resourceType) =
    let mutable assignedTo = new HashSet<Resources<'t>>() // TODO: do this in debug; for release, use just a reference count.
    member val ResourceType: ResourceType = resourceType
    member __.AssignedTo = assignedTo

    member __.AssignTo resources = assignedTo.Add resources

    member __.Reclaim(res) =
        assert assignedTo.Contains res
        ignore <| assignedTo.Remove res // TODO: do this in debug; for release, use just a reference count.

    member __.Value = value

and Resources<'t>() =
    let byKey = new Dictionary<ResourceKey, Resource<'t>>()
    let byType = new Dictionary<ResourceType, list<Resource<'t>>>()

    member __.ComplyWith(requirements: Requirements) =
        Seq.forall
            id
            (seq {
                for kv in byKey do
                    yield requirements.ByKey kv.Key = kv.Value.ResourceType
            })

    member __.ByKey k = byKey[k]
    member __.ByType t = byType[t] :> seq<Resource<'t>>
    member __.Item k = byKey[k]

    member this.Assign(k, resource: Resource<'t>) =
        assert not (byKey.ContainsKey k)
        byKey[k] <- resource
        let t = resource.ResourceType
        byType[t] <- resource :: byType[t]
        resource.AssignTo this

    member this.Reclaim() = // deliberately not fine-grained
        let v = byKey.Values

        for res in v do
            res.Reclaim(this)

        byKey.Clear()
        byType.Clear()
        v
