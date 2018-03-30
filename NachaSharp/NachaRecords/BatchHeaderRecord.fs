namespace NachaSharp

open System
open System.Collections.Generic
open FSharp.Data.FlatFileMeta
open FSharp.Data.FlatFileMeta.MetaDataHelper

type BatchHeaderRecord(rowInput) =
    inherit NachaRecord(rowInput, "5")
    
     static member Create() =
        MetaDataHelper.createRecord BatchHeaderRecord 
            (fun bh ->
                
                ()
            )
    
    override this.Setup () = 
        setup this <|
                lazy ({ 
                         columns =[
                                    MetaColumn.Make( 1, this.RecordTypeCode,     Format.leftPadString)
                                    MetaColumn.Make( 3, this.ServiceClassCode,   Format.leftPadString)
                                    MetaColumn.Make(16, this.CompanyName,        Format.rightPadString)
                                    MetaColumn.Make(20, this.CompanyDiscretionaryData, Format.rightPadString)
                                    MetaColumn.Make(10, this.CompanyIdentification, Format.leftPadString)
                                    MetaColumn.Make( 3, this.StandardEntryClass, Format.leftPadString)
                                    MetaColumn.Make(10, this.CompanyEntryDescription, Format.rightPadString)
                                    MetaColumn.Make( 6, this.CompanyDescriptiveDate, Format.optYYMMDD)
                                    MetaColumn.Make( 6, this.EffectiveEntryDate, Format.reqYYMMDD)
                                    MetaColumn.Make( 3, this.SettlementDate, Format.optJulian)
                                    MetaColumn.Make( 1, this.OriginatorStatusCode, Format.leftPadString)
                                    MetaColumn.Make( 8, this.OriginatingDfiIndentifications, Format.leftPadString)
                                    MetaColumn.Make( 7, this.BatchNumber, Format.zerodInt)
                                  ]
                         length = 94
                     })

    member this.Entries 
        with get () = this.GetChildList<EntryDetail>()
    member this.BatchControl 
        with get () = this.GetChild<BatchControlRecord>(lazy NoRecord)
        and set value = this.SetChild<BatchControlRecord>(value)
        
        
    member this.ServiceClassCode
        with get () = this.GetColumn()
        and set value = this.SetColumn<string> value

    member this.CompanyName
        with get () = this.GetColumn()
        and set value = this.SetColumn<string> value

    member this.CompanyDiscretionaryData
        with get () = this.GetColumn()
        and set value = this.SetColumn<string> value
        
    member this.CompanyIdentification
        with get () = this.GetColumn()
        and set value = this.SetColumn<string> value
        
    member this.StandardEntryClass
        with get () = this.GetColumn()
        and set value = this.SetColumn<string> value
        
    member this.CompanyEntryDescription
        with get () = this.GetColumn()
        and set value = this.SetColumn<string> value       
         
    member this.CompanyDescriptiveDate
        with get () = this.GetColumn()
        and set value = this.SetColumn<DateTime Nullable> value        
  
    member this.EffectiveEntryDate
        with get () = this.GetColumn()
        and set value = this.SetColumn<DateTime> value            
          
    member this.SettlementDate
        with get () = this.GetColumn()
        and set value = this.SetColumn<DateTime Nullable> value          
          
    member this.OriginatorStatusCode
        with get () = this.GetColumn()
        and set value = this.SetColumn<string> value        

    member this.OriginatingDfiIndentifications
        with get () = this.GetColumn()
        and set value = this.SetColumn<string> value
        
    member this.BatchNumber
        with get () = this.GetColumn()
        and set value = this.SetColumn<int> value   