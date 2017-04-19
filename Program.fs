// Learn more about F# at http://fsharp.org

open System
open System.Reflection
open System.Reflection.Emit

open compiler

[<EntryPoint>]
let main argv =
    let aname = new AssemblyName("Test.dll")
    let ab = AssemblyBuilder.DefineDynamicAssembly(aname, AssemblyBuilderAccess.Run)
    let mb = ab.DefineDynamicModule "Test"
    let mainType = mb.DefineType("Test.Test", TypeAttributes.Class ||| TypeAttributes.Public)
    
    let def = { StructDef.name = "mytest"
                targs = ["a"]
                members = ["name", BasicType "string"; "data", BasicType "a"] }

    let comp = { CompilerState.scope = Map.add "string" (ScopeType (IntrospectionExtensions.GetTypeInfo typeof<string>)) Map.empty }

    compile1 comp (TypeStatement (StructTypeDef def)) mainType
    
    printfn "Hello World from F#!"
    0 // return an integer exit code
