module VoxLogicA.ITypedOperators

type ITypedOperators<'t, 'v , 'id> =
    abstract member ConvertImage : 't -> 'id -> 't
    abstract member Multiply : 't * 'v -> 't
    abstract member Multiply : 't * 't -> 't
    abstract member Add : ('t * 'v) -> 't
    abstract member Add : ('t * 't) -> 't