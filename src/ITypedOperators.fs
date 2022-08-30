module VoxLogicA.ITypedOperators

type ITypedOperators<'t, 'v> =
    abstract member Multiply : 't * 'v -> 't
    abstract member Multiply : 't * 't -> 't
    abstract member Add : 't * 'v -> 't
    abstract member Add : 't * 't -> 't