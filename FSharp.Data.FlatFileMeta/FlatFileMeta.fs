(*
   Copyright 2018 EkonBenefits

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

namespace rec FSharp.Data.FlatFileMeta

open System.Collections.Generic
open System.IO
open System
open System.ComponentModel
open System.Runtime.CompilerServices
open FSharp.Interop.Compose.Linq
open System.Runtime.InteropServices


[<AutoOpen>]
module ValidationExtension =
    type ErrorInfo = { row:int; column:int; name:string; description:string }

    type Validation =
        | Okay of obj
        | Error of ErrorInfo list

    type ValidationBuilder() =
        member __.Zero() = Okay ()
        member __.Yield(x) = Okay x
        member __.Yield(x) = Error [x]
        member __.YieldFrom(x:Validation) = x
        member __.Combine(v1:Validation, v2:Validation) = 
            match v1, v2 with
                | Okay _, Okay _-> Okay ()
                | Error x, Okay _
                | Okay _, Error x -> Error x
                | Error x, Error y -> Error (x @ y)
                
        member __.Delay(f) = f
        member __.Run(f) = f()
        
        member this.While(guard, delayedExpr) =
            let mutable result = this.Zero()
            while guard() do
                result <- this.Combine(result,this.Run(delayedExpr))
            result
            
        member this.Using(resource:#IDisposable, body) =
                    this.TryFinally(this.Delay(fun ()->body resource), 
                        fun () -> match box resource with null -> () | _ -> resource.Dispose())  
            
        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(), 
                   fun enum -> 
                        this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))
        
        member this.TryWith(delayedExpr, handler) =
            try this.Run(delayedExpr)
            with exn -> handler exn
            
        member this.TryFinally(delayedExpr, compensation) =
            try this.Run(delayedExpr)
            finally compensation()
            
      

    let validate = ValidationBuilder()
            
    
[<AutoOpen>]
module CreateRowExtension =
    type CreateRowBuilder() =
        member __.Bind(m:string->#FlatRow, f) = 
                null |> m |> f
        member __.Return(x) = x
        
        member __.ReturnFrom(x:string->#FlatRow) = null |> x
    let createRow = new CreateRowBuilder()      

type MaybeRow<'T when 'T :> FlatRow> =
    SomeRow of 'T | NoRow

[<Extension>]
type MaybeRowCSharpExtension =
    [<Extension>]
    static member TrySomeRow<'T when 'T :> FlatRow>
        (this: MaybeRow<'T>, [<System.Runtime.InteropServices.Out>] some : byref<'T> ) : bool =
        match this with
            | NoRow -> some <- Unchecked.defaultof<'T>; false
            | SomeRow x -> some <- x; true
       

[<AutoOpen>]
module MaybeRowExtension =
    type MaybeRowBuilder() =
        member __.Bind(m, f) = 
            Option.bind f m

        member __.Bind(m, f) = 
            match m with
                | SomeRow(r) -> f r
                | NoRow -> None

        member __.Return(x) = Some x

        member __.ReturnFrom(x) = x

        member __.Yield(x) = Some x

        member __.YieldFrom(x) = x
        
        member this.Zero() = this.Return ()
   
        member __.Delay(f) = f

        member __.Run(f) = f()
    
        member this.TryWith(delayedExpr, handler) =
            try this.Run(delayedExpr)
            with exn -> handler exn
        member this.TryFinally(delayedExpr, compensation) =
            try this.Run(delayedExpr)
            finally compensation()
        member this.Using(resource:#IDisposable, body) =
            this.TryFinally(this.Delay(fun ()->body resource), 
                fun () -> match box resource with null -> () | _ -> resource.Dispose())
    

    let maybeRow = new MaybeRowBuilder()       


type ColumnIdentifier(key: string, length:int, placeHolder:bool) =
    member __.Key = key
    member __.Length = length
    member __.PlaceHolder = placeHolder
    
type Column<'T>(key: string, length:int, getValue: Format.FormatGet<'T>, setValue: Format.FormatSet<'T>) =
    inherit ColumnIdentifier(key, length, false)
    member __.GetValue = getValue
    member __.SetValue = setValue


type ProcessedMeta = int * string IList * IDictionary<string, int * ColumnIdentifier>

type DefinedMeta = { columns: ColumnIdentifier list; length :int }


type internal ChildList<'T when 'T :> FlatRow>(parent:FlatRow) =
    inherit System.Collections.ObjectModel.Collection<'T>()
    override this.InsertItem(index, item) =
        item.Parent <- SomeRow(parent)
        base.InsertItem(index, item)
        this.NotifyChanged(item)
    override this.SetItem(index, item) =
        item.Parent <- SomeRow(parent)
        base.SetItem(index, item)
        this.NotifyChanged(item)
    member __.NotifyChanged(item) =
        if item.HelperGetAllowMutation () then
            maybeRow {
                let! parent = item.Parent
                parent.Changed()
            } |> ignore


[<RequireQualifiedAccess>]
module MaybeRow =

      [<CompiledName("Cast")>]  
      let cast (m:MaybeRow<FlatRow>) : MaybeRow<#FlatRow> =
           match m with
              | SomeRow(r) -> SomeRow(downcast r)
              | NoRow -> NoRow
       
      [<CompiledName("Cast")>]  
      let generalize (m:MaybeRow<#FlatRow>) : MaybeRow<FlatRow> =
         match m with
            | SomeRow(r) -> SomeRow(upcast r)
            | NoRow -> NoRow
              
      [<CompiledName("IsSomeRow")>]      
      let isSomeRecord =
           function 
                | SomeRow _ -> true
                | NoRow -> false

      [<CompiledName("IsNoRow")>]      
      let isNoRecord =
           function 
                | SomeRow _ -> false
                | NoRow -> true
        

      [<CompiledName("ToOption")>]        
      let toOption<'T when 'T :> FlatRow> (maybeRec: MaybeRow<'T>) : Option<'T> =
            match maybeRec with 
                | SomeRow x -> Some(x)
                | NoRow -> None
        
      [<CompiledName("OfOption")>]        
      let ofOption (opt:#FlatRow option) =
            match opt with  
                | Some x -> SomeRow(x)
                | None -> NoRow       
          

                     
 type Hierarchy = 
       Child of (FlatRow MaybeRow)
       | Children of (System.Collections.IEnumerable)
       | Other of (obj)
    
 type RankedHierarchy = Ranked of int * Hierarchy
      
[<AbstractClass>]
type FlatRow(rowData:string) =
    let rowInput = Helper.optionOfStringEmpty rowData
    let mutable rawData: string array = Array.empty
    let mutable columnKeys: IList<string> = upcast List()
    let mutable columnMap: IDictionary<string, int * ColumnIdentifier> = upcast Map.empty 
    let mutable columnLength: int = 0
    let mutable allowMutation = rowInput.IsNone
    let mutable calculating = false
    let children = Dictionary<string,RankedHierarchy>()
    
    member val Parent: FlatRow MaybeRow = NoRow with get,set
    
    member this.Root:FlatRow MaybeRow = 
            let rec findRoot (f:FlatRow MaybeRow) (l:FlatRow MaybeRow) =
                match f with 
                    | NoRow -> l
                    | SomeRow(p) -> findRoot p.Parent (SomeRow(p))
            findRoot this.Parent NoRow
    
    member val ParsedLineNumber: int option = None with get,set
    
    

    
    member this.AllowMutation with get () = allowMutation
                              and set value =
                                            allowMutation <- value
                                            if allowMutation then
                                                this.Changed()
                     
    member __.IsNew() = rowInput.IsNone

    abstract Setup: unit -> ProcessedMeta
    
    abstract PostSetup: unit -> unit
    
    abstract CalculateImpl: unit -> unit
    
    member this.Calculate() =
         if not calculating then
             try
                   calculating <- true
                   this.CalculateImpl()
             finally
                   calculating <- false      
    
    default this.CalculateImpl () =
        if not <| this.HelperGetAllowMutation ()  then
            invalidOp "AllowMutation is not set on root"

        children.Keys
            |> Seq.map this.ChildData
            |> Seq.iter (function | Child(mf) -> maybeRow {
                                                                let! f = mf
                                                                f.Calculate()
                                                            } |> ignore
                                  | Children (l) -> 
                                        l |> Enumerable.ofType<FlatRow>
                                          |> Seq.iter (fun i->i.Calculate())
                                  | _ ->())

                                      
    member this.Changed() =
                this.Root |> function | SomeRow(r) -> r.Changed() 
                                      | NoRow -> this.Calculate()
    
    member this.IsMatch() = this.DoesLengthMatch() && this.IsIdentified ()
    
    abstract IsIdentified: unit -> bool
    
    member this.DoesLengthMatch () = this.Row |> Array.length = columnLength

    member private this.LazySetup() =
        if columnMap.Count = 0 then
            let totalLength, orderedKeys, mapMeta = this.Setup()
      
            columnLength <- totalLength
            columnKeys <- orderedKeys
            rawData <- match rowInput with
                        | Some (row) -> 
                            row.PadRight totalLength
                            |> Array.ofSeq |> Array.map string
                        | None -> Array.init totalLength (fun _ -> " ")
            columnMap <- mapMeta
            this.PostSetup()

       
    member __.ChildKeys() =
        children 
            |> Seq.sortBy (fun kp -> match kp.Value with |Ranked(r,_)->r) 
            |> Seq.map (fun kp -> kp.Key)
        
    member __.ChildData(key:string):Hierarchy=
        children.[key] |> (function | Ranked(_,h) -> h)
     
    member this.Keys() =
        this.LazySetup()
        columnKeys   
        
    member private this.Row =
        this.LazySetup()
        rawData
    
    member private this.ColumnMap =
        this.LazySetup()
        columnMap
    
    member this.Data(key:string):obj=
        this.GetColumn(key) |> box
        
    member this.ToRawString() =
        this.Row |> String.concat ""
    
    member this.RawData(key:string)=
        let start, columnIdent = this.ColumnMap.[key]
        let endSlice = start - 1 + columnIdent.Length
        this.Row.[start..endSlice] |> String.concat ""             
            
    member this.MetaData(key:string) =
        let start, columnIdent = this.ColumnMap.[key]
        struct (start, columnIdent.Length)
          
    member internal this.HelperGetAllowMutation () =
        maybeRow { let! root = this.Root
                   return root.AllowMutation
                 } |> Option.defaultWith (fun ()-> this.AllowMutation)
     
    member private __.HelperGetChild<'T when 'T :> FlatRow> (rank:int) (key:string) : MaybeRow<'T> =
                match children.TryGetValue(key) with
                    | true,Ranked(_,Child(mv)) -> mv |> MaybeRow.cast
                    | true,_ -> invalidOp "Different type for this name already added"
                    | ______ -> let d = Child(NoRow)
                                children.[key] <- Ranked(rank, d)
                                NoRow
                                
    member private __.HelperGetOther (rank:int) (defaultValue:'T Lazy) (key:string) =
                    match children.TryGetValue(key) with
                        | true,Ranked(_,Other(v)) -> downcast v
                        | true,_ -> invalidOp "Different type for this name already added"
                        | ______ -> let d = defaultValue.Force()
                                    children.[key] <- Ranked(rank, Other(rank, box d))
                                    d
                                
    member private this.HelperGetChildren<'T when 'T :> FlatRow> (rank:int) (key:string) : 'T IList  =
              match children.TryGetValue(key) with
                  | true,Ranked(_,Children(v)) -> downcast v
                  | true,_ -> invalidOp "Different type for this name already added"
                  | ______ -> let d: 'T IList = upcast ChildList<'T>(this)
                              let h = Children(d)
                              children.[key] <- Ranked(rank,h)
                              d                      
                                
    member this.GetChild(rank:int, [<CallerMemberName>] ?memberName: string) : #FlatRow MaybeRow= 
            let key =  memberName
                       |> Option.defaultWith Helper.raiseMissingCompilerMemberName
            this.HelperGetChild rank key
            
    member this.GetChildList(rank:int,[<CallerMemberName>] ?memberName: string) : #FlatRow IList = 
                let key =  memberName
                           |> Option.defaultWith Helper.raiseMissingCompilerMemberName
                this.HelperGetChildren rank key   
                                   
    member this.SetChild(rank:int, value:#FlatRow MaybeRow, [<CallerMemberName>] ?memberName: string) : unit = 
                let key = 
                    memberName
                       |> Option.defaultWith Helper.raiseMissingCompilerMemberName
                let mcr = value |> MaybeRow.generalize
                maybeRow{
                    let! cr = mcr
                    cr.Parent <- SomeRow(this)
                } |> ignore
                children.[key] <- Ranked(rank,Child(mcr))
                if this.HelperGetAllowMutation () then
                    this.Changed()
         
    member this.SetOther(rank:int, value:obj, [<CallerMemberName>] ?memberName: string) : unit = 
               let key = 
                   memberName
                      |> Option.defaultWith Helper.raiseMissingCompilerMemberName
               children.[key] <- Ranked(rank,Other(value))
               if this.HelperGetAllowMutation () then
                   this.Changed()
            
    member this.GetColumn([<CallerMemberName>] ?memberName: string) : 'T =
        let start, columnIdent =
            match memberName with
                | Some(k) -> 
                        try 
                            this.ColumnMap.[k]
                        with exn -> raise <| KeyNotFoundException(sprintf "Schema missing %s in %A" k this, exn)
                | None -> Helper.raiseMissingCompilerMemberName()
        let endSlice = start - 1 + columnIdent.Length 
        let slice = this.Row.[start..endSlice]
        let data = slice |> String.concat ""
        let columnDef:Column<'T> = downcast columnIdent
        try 
            data |> columnDef.GetValue 
        with
            exn -> 
                    match this.ParsedLineNumber with
                        | Some(ln) -> 
                            raise <| InvalidDataException(sprintf "Unexpected value '%s' for '%s' at record %i %s" 
                                                                data columnIdent.Key ln (this.GetType().Name), exn)
                        | None ->
                            raise <| InvalidDataException(sprintf "Unexpected value '%s' for '%s'" data columnIdent.Key , exn)
    

    member this.SetColumn<'T>(value:'T, [<Optional; DefaultParameterValue(false)>]autoTrim: bool, [<CallerMemberName; Optional; DefaultParameterValue("")>] ?memberName: string) =
        let start, columnIdent =
            match memberName with
                 | Some(k) ->
                    try 
                        this.ColumnMap.[k]
                    with exn -> raise <| KeyNotFoundException(sprintf "Schema missing %s in %A" k this, exn)
                 | None -> Helper.raiseMissingCompilerMemberName()
        if not <| this.HelperGetAllowMutation ()  then
            invalidOp "AllowMutation is not set on root"
                 
        let columnDef:Column<'T> = downcast columnIdent
        let stringVal = value |> columnDef.SetValue (columnIdent.Length, autoTrim)
        let newSlice = stringVal.ToCharArray() |> Array.map string
        let endSlice = start - 1 + columnIdent.Length
        this.Row.[start..endSlice] <- newSlice
        this.Changed()
 
 