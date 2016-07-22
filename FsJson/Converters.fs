module FsJson.Converters
// Inspired by: https://github.com/haf/Newtonsoft.Json.FSharp

open System
open FSharp.Reflection
open Newtonsoft.Json
open System.Globalization
open System.Numerics
open System.Collections.Generic

let private expect (reader: JsonReader) (tokenType: JsonToken) =
    if reader.TokenType <> tokenType
    then do failwithf "Expected '%A' but '%A' found at '%s'" tokenType reader.TokenType reader.Path

let private unexpected (reader: JsonReader) =
    do failwithf "Unexpected token '%A' found at '%s'" reader.TokenType reader.Path

let private readExpect (reader: JsonReader) (tokenType: JsonToken) =
    if not (reader.Read ()) 
    then do failwithf "Expected '%A' but EOF found at '%s'" tokenType reader.Path
    else expect reader tokenType

let private readAny (reader: JsonReader) =
    if not (reader.Read ()) 
    then do failwithf "Unexpected EOF found at '%s'" reader.Path

let private adaptToReader reader =
    (
        expect reader, 
        readExpect reader,
        (fun () -> readAny reader),
        (fun () -> unexpected reader)
    ) 

let private memoize1 f =
    let cache = Dictionary<_, _>()
    fun x ->
        let found, cached = cache.TryGetValue x
        if found 
        then cached
        else 
            let cached = f x
            do cache.[x] <- cached
            cached

(**
- Convert options as if they where nullable values or just plain reference types
- Ex: None <-> "null"
      Some 42 <-> "42"
*)
type FsOptionConverter() = 
    inherit JsonConverter()

    let typeOfOption = typedefof<option<_>>
    let typeOfNullable = typedefof<Nullable<_>>

    let mapIfValue predicate mapper value =
        if predicate value
        then mapper value
        else value

    override this.WriteJson (writer, value, serializer) =
        if value = null
        then writer.WriteNull ()
        else
            let _, values = FSharpValue.GetUnionFields (value, value.GetType())
            serializer.Serialize(writer, values.[0]);
        do ()

    override this.ReadJson (reader, destinationType, existingValue, serializer) =
        let valueType = 
            destinationType.GetGenericArguments().[0]
            |> mapIfValue (fun t -> t.IsValueType) (fun t -> typeOfNullable.MakeGenericType(t))

        let value = serializer.Deserialize(reader, valueType)
        let cases = FSharpType.GetUnionCases destinationType

        if value = null
        then FSharpValue.MakeUnion (cases.[0], [||])
        else FSharpValue.MakeUnion (cases.[1], [|value|])
    
    override this.CanConvert objectType = 
        objectType.IsGenericType 
        && objectType.GetGenericTypeDefinition().Equals typeOfOption

(**
- Convert Tuples to and from Json arrays
- Ex: (42, "a string") <-> "[42, "a string"]"
*)
type FsTupleArrayConverter() = 
    inherit JsonConverter()

    override this.WriteJson (writer, value, serializer) =
        let fields = FSharpValue.GetTupleFields(value)
        do serializer.Serialize (writer, fields)

    override this.ReadJson (reader, destinationType, existingValue, serializer) =
        let expect, readExpect, readAny, unexpected = adaptToReader reader

        do expect JsonToken.StartArray

        let types = FSharpType.GetTupleElements destinationType
        let length = types.Length
        let values = Array.create<obj> length null

        for i = 0 to length - 1 do
            do readAny ()

            let t = types.[i]
            match reader.TokenType with
            | JsonToken.StartObject
            | JsonToken.StartArray
            | JsonToken.Boolean
            | JsonToken.Bytes
            | JsonToken.Date
            | JsonToken.Float
            | JsonToken.Integer
            | JsonToken.String ->
                do values.[i] <- serializer.Deserialize(reader, t)
            | JsonToken.Null ->
                do values.[i] <- null
            | token ->
                do unexpected ()

        do readExpect JsonToken.EndArray
        FSharpValue.MakeTuple(values, destinationType)
    
    override this.CanConvert objectType = 
        FSharpType.IsTuple objectType


(**
- Convert Guids 
- Ex: Guid.Empty <-> "null" or "\"\""
      {B2F83697-D509-4C52-8BA9-D6A0961181C5} <-> ""B2F83697-D509-4C52-8BA9-D6A0961181C5""
*)
type FsGuidConverter() = 
    inherit JsonConverter()

    let typeOfGuid = typedefof<Guid>
    let typeOfString = typedefof<string>

    override this.WriteJson (writer, value, serializer) =
        let guid = value :?> Guid
        if guid = Guid.Empty
        then do writer.WriteValue(String.Empty)
        else do writer.WriteValue(guid.ToString "D")

    override this.ReadJson (reader, destinationType, existingValue, serializer) =
        let str = serializer.Deserialize(reader, typeOfString) :?> string
        if str = null || str = String.Empty
        then Guid.Empty :> obj
        else Guid str :> obj
    
    override this.CanConvert objectType = 
        objectType = typeOfGuid


(**
- Convert Guids 
- Ex: Guid.Empty <-> "null" or "\"\""
      {B2F83697-D509-4C52-8BA9-D6A0961181C5} <-> ""B2F83697-D509-4C52-8BA9-D6A0961181C5""
*)
type FsBigIntConverter() = 
    inherit JsonConverter()

    let typeOfBigInt = typedefof<bigint>
    let typeOfString = typedefof<string>

    override this.WriteJson (writer, value, serializer) =
        let bigie = value :?> BigInteger
        do writer.WriteValue(bigie.ToString ())

    override this.ReadJson (reader, destinationType, existingValue, serializer) =
        let str = serializer.Deserialize(reader, typeOfString) :?> string
        BigInteger.Parse str :> obj
    
    override this.CanConvert objectType = 
        objectType = typeOfBigInt


(**
- Convert CultureInfos 
- Ex: CultureInfo.InvariantCulture <-> "null" or "\"\""
      CultureInfo.GetCultureInfo("es-ES") <-> "\"es-ES\""
*)
type FsCultureInfoConverter() = 
    inherit JsonConverter()

    let typeOfCultureInfo = typedefof<CultureInfo>
    let typeOfString = typedefof<string>

    override this.WriteJson (writer, value, serializer) =
        if value = null
        then do writer.WriteNull()
        else
            let culture = value :?> CultureInfo
            if Object.Equals(culture, CultureInfo.InvariantCulture)
            then 
                do writer.WriteValue(String.Empty)
            else do writer.WriteValue(culture.Name)

    override this.ReadJson (reader, destinationType, existingValue, serializer) =
        let str = serializer.Deserialize(reader, typeOfString) :?> string
        if str = null 
        then null
        elif str = String.Empty
        then CultureInfo.InvariantCulture :> obj
        else CultureInfo.GetCultureInfo str :> obj
    
    override this.CanConvert objectType = 
        objectType = typeOfCultureInfo


(**
- Convert Simple union types 
- Ex: SimpleUnion.Case1 <-> ""Case1""
*)
type FsSimpleUnionConverter() = 
    inherit JsonConverter()

    static let getCasesFunc (t: Type) = FSharpType.GetUnionCases t
    static let getCases = memoize1 getCasesFunc

    static let isSimpleUnionFunc (t: Type) =
        if FSharpType.IsUnion t
        then getCases t |> Seq.forall (fun c -> c.GetFields().Length = 0)
        else false

    static let isSimpleUnion = memoize1 isSimpleUnionFunc

    let typeOfString = typedefof<string>

    override this.WriteJson (writer, value, serializer) =
        let case, _ = FSharpValue.GetUnionFields(value, value.GetType())
        do writer.WriteValue(case.Name);

    override this.ReadJson (reader, destinationType, existingValue, serializer) =
        let str = serializer.Deserialize(reader, typeOfString) :?> string
        let cases = getCases destinationType
        let strongCandidates = cases |> Seq.filter (fun c -> StringComparer.Ordinal.Compare(c.Name, str) = 0)
        let weakCandidates = cases |> Seq.filter (fun c -> StringComparer.OrdinalIgnoreCase.Compare(c.Name, str) = 0)
        let candidates = Seq.concat [strongCandidates; weakCandidates] |> Seq.toList
        match candidates with
        | [] -> failwithf "Could not find any match for '%s' on union type '%s'" str destinationType.FullName
        | first :: _ -> FSharpValue.MakeUnion(first, [||])
    
    override this.CanConvert objectType = 
        isSimpleUnion objectType

(**
- Convert Complex union types 
- Ex: ComplexUnion.Case1 val1 <-> "{"Case1":val1}"
- Ex: ComplexUnion.Case2 (val1, val2, val3) <-> "{"Case1":[val1,val2,val3]}"
*)
type FsComplexUnionConverter() = 
    inherit JsonConverter()

    static let getCasesFunc (t: Type) = FSharpType.GetUnionCases t
    static let getCases = memoize1 getCasesFunc

    static let isListType (t: Type) = 
        t.IsGenericType 
        && t.GetGenericTypeDefinition().Equals(typedefof<list<_>>)

    static let isComplexUnionFunc (t: Type) =
        if FSharpType.IsUnion t && not(isListType t)
        then getCases t |> Seq.exists (fun c -> c.GetFields().Length > 0)
        else false

    static let isComplexUnion = memoize1 isComplexUnionFunc

    let typeOfString = typedefof<string>

    override this.WriteJson (writer, value, serializer) =
        let valueType = value.GetType()
        let case, fields = FSharpValue.GetUnionFields(value, valueType)
        let fieldNames = case.GetFields() |> Seq.map (fun p -> p.Name)
        let map = Seq.zip fieldNames fields |> Map.ofSeq
        do writer.WriteStartObject()
        do writer.WritePropertyName(case.Name)
        do serializer.Serialize (writer, map)
        do writer.WriteEndObject()

    override this.ReadJson (reader, destinationType, existingValue, serializer) =
        let expect, readExpect, readAny, unexpected = adaptToReader reader

        do expect JsonToken.StartObject
        do readExpect JsonToken.PropertyName

        let propertyName = reader.Value :?> string

        do readExpect JsonToken.StartObject

        let cases = getCases destinationType
        let strongCandidates = cases |> Seq.filter (fun c -> StringComparer.Ordinal.Compare(c.Name, propertyName) = 0)
        // let weakCandidates = cases |> Seq.filter (fun c -> StringComparer.OrdinalIgnoreCase.Compare(c.Name, propertyName) = 0)
        let candidates = strongCandidates |> Seq.toList // Seq.concat [strongCandidates; weakCandidates] |> Seq.toList
        let case = 
            match candidates with
            | [] -> failwithf "Could not find any match for '%s' on union type '%s'" propertyName destinationType.FullName
            | first :: _ -> first

        let properties = case.GetFields()
        let length = properties.Length
        let values = Array.create<obj> length null
        let found = Array.create<bool> length false
        let mutable continueLooping = length > 0
        let mutable foundCount = 0

        while continueLooping do
            do readExpect JsonToken.PropertyName
            let name = reader.Value :?> string
            do readAny ()

            let index = Array.FindIndex(properties, (fun p -> p.Name = name))
            if index < 0 then failwithf "Could not find any property '%s' on case '%s' for union type '%s' at path '%s'" name case.Name destinationType.FullName reader.Path
            if found.[index] then failwithf "Property '%s' was found multiple times on case '%s' for union type '%s' at path '%s'" name case.Name destinationType.FullName reader.Path
            do found.[index] <- true
            foundCount <- foundCount + 1

            let prop = properties.[index]
            match reader.TokenType with
            | JsonToken.StartObject
            | JsonToken.StartArray
            | JsonToken.Boolean
            | JsonToken.Bytes
            | JsonToken.Date
            | JsonToken.Float
            | JsonToken.Integer
            | JsonToken.String ->
                do values.[index] <- serializer.Deserialize(reader, prop.PropertyType)
            | JsonToken.Null ->
                do values.[index] <- null
            | token ->
                do unexpected ()

            if foundCount >= length then do continueLooping <- false

        do readExpect JsonToken.EndObject
        do readExpect JsonToken.EndObject

        FSharpValue.MakeUnion(case, values)
    
    override this.CanConvert objectType = 
        isComplexUnion objectType


let setupFsConverters (settings: JsonSerializerSettings) =
    settings.Converters.Add (FsGuidConverter())
    settings.Converters.Add (FsCultureInfoConverter())
    settings.Converters.Add (FsBigIntConverter())
    settings.Converters.Add (FsOptionConverter())
    settings.Converters.Add (FsTupleArrayConverter())
    settings.Converters.Add (FsSimpleUnionConverter())
    settings.Converters.Add (FsComplexUnionConverter())
