namespace VoxLogicA

[<AbstractClass>]
type IModel() =
    inherit Coreops()
    abstract member SetBaseImage : string -> Hopac.Job<unit>
    [<OperatorAttribute("load","string","model","load an image")>]
    abstract member Load : string -> Hopac.Job<obj>
    abstract member Save : string -> obj -> Hopac.Job<float * float>
    abstract member CanSave : Type -> string -> bool
    abstract member SignalNumThreads : int -> Hopac.Job<unit>
