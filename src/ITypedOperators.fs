module VoxLogicA.ITypedOperators

type ITypedOperators =
    abstract member ConvertImage : 't -> 'v -> 't
    abstract member Multiply : 't -> 't
    abstract member Add : 't -> 't