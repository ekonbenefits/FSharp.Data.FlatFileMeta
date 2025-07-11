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

open FSharp.Control
open System.Runtime.CompilerServices
open System.IO
open System.Collections.Generic
open FSharp.Interop.Compose.Linq
open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

[<RequireQualifiedAccess>]
module FlatRowProvider =   
    open System.Text
    open System.Collections.Concurrent

    type internal WriteState = { node: FlatRow; accum: FlatRow list }
    
    let asyncWriteFileRepeating (newlineTerm:string) (head:#FlatRow seq) (stream:Stream) =
        
        let rec foldFlat (state:FlatRow list) (item:FlatRow) =
            let children = 
                item.ChildKeys()
                   |> List.ofSeq
                   |> List.collect(fun key ->
                          match item.ChildData(key) with
                          | Other(_) -> []
                          | Children (l) ->
                              l |> Enumerable.ofType<FlatRow> 
                                |> Seq.fold foldFlat []
                          | Child (mc) -> match mc with
                                          | NoRow -> []
                                          | SomeRow(c) -> foldFlat [] c)
                   
            state @ (item::children)
        let flatList = head |> Seq.collect (foldFlat [])
        use writer = new StreamWriter(stream, Encoding.ASCII, 1024, true)
        writer.NewLine <- newlineTerm
        async {
            do! flatList 
                |> AsyncSeq.ofSeq
                |> AsyncSeq.iterAsync(fun row -> row.ToRawString() 
                                                    |> writer.WriteLineAsync
                                                    |> Async.AwaitTask)
            do! writer.FlushAsync() |> Async.AwaitTask                                      
        }

    let asyncWriteFile (newlineTerm:string) (head:#FlatRow) (stream:Stream) = asyncWriteFileRepeating newlineTerm [head] stream

    let syncWriteFile newLineTerm (head:#FlatRow) (stream:Stream)  = 
         asyncWriteFile newLineTerm head stream |> Async.RunSynchronously

    let syncParseLines (parser:string AsyncSeq -> #FlatRow MaybeRow Async) = 
            AsyncSeq.ofSeq >> parser >> Async.RunSynchronously
            
    let internal asyncParseFileGeneric (parser:string AsyncSeq -> 'a Async) (stream:Stream) =
        let seq = asyncSeq{
                        use reader = new StreamReader(stream,
                                                            Encoding.ASCII, 
                                                            false, 
                                                            1024, 
                                                            true)
                        let mutable completed = false
                        while not (completed) do 
                            let! line = reader.ReadLineAsync() |> Async.AwaitTask
                            let found = line |> Option.ofObj
                            match found with
                                | Some(line) ->
                                    yield line
                                | None -> completed <- true
                  }

        seq |> parser

    let asyncParseFileRepeating (parser:string AsyncSeq -> #FlatRow seq Async) (stream:Stream) =
          asyncParseFileGeneric parser stream

    let asyncParseFile (parser:string AsyncSeq -> #FlatRow MaybeRow Async) (stream:Stream) =
         asyncParseFileGeneric parser stream
      
     
    let syncParseFile parser stream = 
         asyncParseFile parser stream |> Async.RunSynchronously

    let matchRecord (constructor:string -> #FlatRow) lineNumber value  =
        let result = value |> constructor       
        if result.IsMatch() && not <| result.IsNew() then
            result.ParsedLineNumber <- Some(lineNumber)
            Some(result)
        else
            None
           
    let multiMatch (matchers:(int -> string -> #FlatRow option) list) lineNo value =
        matchers
            |> List.map (fun f -> f lineNo value)
            |> List.tryFind (fun x->x.IsSome)
            |> Option.flatten

    let internal cache = ConcurrentDictionary<Type,ProcessedMeta>()

    let setup (row:#FlatRow) (v: DefinedMeta Lazy) : ProcessedMeta =
        let k = row.GetType()
        if cache.ContainsKey(k) then
            cache.[k]
        else
                let meta = v.Force()
                let sumLength = meta.columns |> List.sumBy (fun x->x.Length)
                if sumLength <> meta.length then
                    raise <| InvalidDataException(sprintf "Data columns sum to %i which is not the expected value %i of %A" sumLength meta.length row)
                
                let realKeys = meta.columns 
                                |> Seq.filter (fun x->not x.PlaceHolder) 
                                |> Seq.map (fun x->x.Key)
                                |> Enumerable.toList
                                :> IList<_>
                let fullIdentifiers =
                     meta.columns 
                         |> Seq.scan (fun state i -> i.Length + state) 0
                         |> Seq.zip meta.columns
                         |> Seq.map (fun (c, i) -> c.Key, (i,c))
                
                
                let result = {
                                  rowLength = meta.length
                                  orderedColumns = realKeys
                                  columnMap = fullIdentifiers |> Map.ofSeq :> IDictionary<_,_>
                                  orderedRawColumns = fullIdentifiers |> Seq.map(fst) |> Enumerable.toList :> IList<_>
                              }
                              
                let keys = result.orderedRawColumns
                if keys |> Seq.distinct |> Seq.length <> keys.Count then
                    raise <| InvalidDataException(sprintf "duplicate column names defined in %A" row)
                cache.[k] <- result
                result      
            




[<AutoOpen>]
module setupExtensions =

    [<AbstractClass>]
    type SetupMetaBuilderBase(fr:FlatRow) = 
        member __.Yield(x) = {columns = [];length =0}
        
        member __.Delay(x) = lazy (x())
                
        [<CustomOperation("checkLength")>] 
        member __.CheckLength (meta, x) = {meta with length = x }
    
        /// Defines width of data to ignore
        [<CustomOperation("placeholder")>] 
        member __.Placeholder (meta, length) =
           { meta with columns = meta.columns @ [ColumnIdentifier(Guid.NewGuid().ToString("D"), length, true)]}

        /// Defines width of property and how to format.
        [<CustomOperation("columns")>] 
        member __.Columns (meta : DefinedMeta, length, [<ReflectedDefinition>] value:Expr<'T> , (getValue: string -> 'T, setValue)) = 
           let key = 
                match value with
                | PropertyGet(_, propOrValInfo, _) -> propOrValInfo.Name
                | ________________________________ -> invalidArg "value" "Must be a property get"
            
           { meta with columns = meta.columns @ [Column(key, length, getValue, setValue)]}
           
      
    type SetupMetaBuilder(fr:FlatRow) = 
        inherit SetupMetaBuilderBase(fr)
        
        member __.Run(x) =
            FlatRowProvider.setup fr x
               
    let setupMetaFor (fr) = SetupMetaBuilder(fr)
    
    type SetupMetaBuilderRaw(fr:FlatRow) = 
        inherit SetupMetaBuilderBase(fr)
        
        member __.Run(x: DefinedMeta Lazy) =
           x 

    let preSetupMetaFor (fr) = SetupMetaBuilderRaw(fr)