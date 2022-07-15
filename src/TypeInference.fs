module VoxLogicA.TypeInference

open Parser

type exprType = 
    TNumber | TBool | TString | TFun of List<exprType> * exprType

type typeEnv = unit

let rec inferenceExpr : typeEnv -> Expression -> exprType = 
    fun env expr -> 
        match expr with
        | ENumber _ -> TNumber
        | EBool _ -> TBool
        | EString _ -> TString
        | ECall (pos,string,args) ->
            let argTypes = List.map (inferenceExpr env) args
            failwith ""

