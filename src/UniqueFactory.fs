namespace VoxLogicA

exception UniqueFactoryException
    with override this.Message = "Internal error in module UniqueFactory.fs"

open System
type UniqueFactory<'Key,'Value when 'Key : equality>() =
    let mutable curId = 0
    let dict = new System.Collections.Generic.Dictionary<'Key,(int * 'Value)>()
    let mutable values = Array.create 1000 None
  
    let create k maker =         
        let newId = curId
        curId <- curId + 1 // The whole application crucially depends on the fact that ids are allocated sequentially.
        let created = maker newId
        dict.[k] <- (newId,created)
        let l = values.Length
        if newId >= l 
        then values <- Array.init (2 * l) (fun id -> if id < l then values.[id] else None)
        values.[newId] <- Some created
        (newId,created)    
    
    //member __.Values = Array.ofSeq (Seq.map snd (Seq.sortBy fst dict.Values)) // TODO do we need to keep this stored instead of computing it each time?

    member __.Count = curId
    member __.Item i = 
        if i < curId 
        then match values.[i] with 
                | None -> raise UniqueFactoryException 
                | Some x -> x
        else raise (IndexOutOfRangeException())
        
    member __.Add(k,maker) = // returns the created element
        if dict.ContainsKey(k) 
        then dict.[k]
        else create k maker
