namespace VoxLogicA

open Microsoft.FSharp.Quotations

type Arithmetics() =

    member __.Number(i: float) = i
    member __.Plus (i: float,j: float) = i + j

    static member F() =
        let a = new Arithmetics()

        let t = a.GetType()

        let q =
            // let x = Expr.Call (t.GetMethod "Plus",[Expr.Value a; Expr.Value 3.0; Expr.Value 3.0]) 
            // printfn $"{x}"
            <@ 
                a.%%"Plus"
            @>

        let res = Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation q 
        res :?> float



