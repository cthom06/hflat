module compiler

open System.Reflection
open System.Reflection.Emit

type Expression = 
    | Ident of string
    | Call of name : string * args : Expression list
    | IntLit of int
    | StrLit of string
    | Match of matches : MatchExpression list
and MatchExpression = {
    name : string
    args : string list
    body : Expression
    }

type TypeExp =
    | BasicType of string
    | GenericType of string * (TypeExp list) 

type FuncDef = {
    name : string
    targs : string list
    constraints : (string * string) list
    args : (string * TypeExp) list
    ret : TypeExp Option
    }

type FuncBodyDef = {
    name : string
    args : string list
    body : Expression
    }

type ClassDef = {
    name : string
    tname : string
    members : FuncDef list
    }

type StructDef = {
    name : string
    targs : string list
    members : (string * TypeExp) list
    }

type UnionDef = {
    name : string
    targs : string
    members : TypeDef list
    }

and TypeDef =
    | FuncType of argTypes : TypeExp list * retType : TypeExp
    | StructTypeDef of StructDef
    | UnionTypeDef of UnionDef

type Statement =
    | FuncStatement of func : FuncDef
    | ClassStatement of cls : ClassDef
    | TypeStatement of typ : TypeDef

type ScopeItem = 
    | ScopeType of TypeInfo
    | ScopeFunc of MethodInfo

type CompilerState = {
    mutable scope : Map<string,ScopeItem>
}

let getType (c : CompilerState) (def : TypeExp) : TypeInfo Option =
    match def with
    | BasicType name ->
        match Map.tryFind name c.scope with
        | Some (ScopeType t) -> Some t
        | _ -> None
    | GenericType (name, prms) ->
        match Map.tryFind name c.scope with
        | Some (ScopeType t) ->
            None
        | _ -> None

let compile1 (c : CompilerState) (s : Statement) (t : TypeBuilder) =
    match s with
    | TypeStatement (StructTypeDef st) ->
        let ti = t.DefineNestedType (st.name, TypeAttributes.NestedPublic)
        c.scope <- Map.add st.name (ScopeType ti) c.scope
        let tempC = { c with scope = c.scope }
        if not (List.isEmpty st.targs) then
            for ai in ti.DefineGenericParameters (Array.ofSeq st.targs) do
                tempC.scope <- Map.add ai.Name (ScopeType ai) tempC.scope
        for (name, typ) in st.members do
            match getType tempC typ with
            | Some typ ->
                ti.DefineField (name, typ.AsType(), FieldAttributes.Public)
                |> ignore
            | None -> raise <| new System.Exception ("bad type")
(*    | TypeStatement (UnionTypeDef un) ->
        let ti = t.DefineNestedType (un.name, TypeAttributes.NestedPublic)
        c.scope <- Map.add un.name (ScopeType ti) c.scope
        let tempC = { c with scope = c.scope }
        if not (List.isEmpty st.targs) then
            for ai in ti.DefineGenericParameters (Array.ofSeq un.targs) do
                tempC.scope <- Map.add ai.Name (ScopeType ai) tempC.scope *)
    | _ -> ()
    
