namespace FsJson.Tests

open System
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Converters

open FsUnit
open NUnit.Framework

open FsJson.Converters
open FsJson.Tests.TestTypes
open System.Globalization
open System.Numerics

[<TestFixture>]
type ``FsJson Converters Tests``() = 
    
    let newSettings() =
        let settings = JsonSerializerSettings()
        settings.Formatting <- Formatting.None
        setupFsConverters settings
        settings

    let serialize value =
        let settings = newSettings()
        JsonConvert.SerializeObject(value, settings)

    let deserialize text =
        let settings = newSettings()
        JsonConvert.DeserializeObject<'t>(text, settings)

    let deserializeBy text valueType =
        let settings = newSettings()
        JsonConvert.DeserializeObject(text, valueType, settings)

    let testSerialize (value: 'a) (text: string) =
        let result = serialize value
        do result |> should be (equal text)

    let testDeserialize (value: 'a) text =
        let result: 'a = deserialize text
        do result |> should be (equal value)

    let testSerialization (value: 'a) text =
        testSerialize value text
        testDeserialize value text

    [<Test>] 
    member test.``FsJson: Serialization Option None``() = 
        let value: string option = None
        do testSerialization value "null"

    [<Test>] 
    member test.``FsJson: Serialization Option Some string``() = 
        let value = Some "a string"
        do testSerialization value "\"a string\""

    [<Test>] 
    member test.``FsJson: Serialization Option Some integer``() = 
        let value = Some 42
        do testSerialization value "42"

    [<Test>] 
    member test.``FsJson: Serialization Tuple of two``() = 
        do testSerialization (42, "a string") """[42,"a string"]"""

    [<Test>] 
    member test.``FsJson: Serialization BigInteger``() = 
        do testSerialization (BigInteger(-123456789)) "\"-123456789\""

    [<Test>] 
    member test.``FsJson: Serialization Guid.Empty``() = 
        do testSerialization Guid.Empty "\"\""

    [<Test>] 
    member test.``FsJson: Serialization Guid``() = 
        do testSerialization (Guid("b2f83697-d509-4c52-8ba9-d6a0961181c5")) "\"b2f83697-d509-4c52-8ba9-d6a0961181c5\""

    [<Test>] 
    member test.``FsJson: Deserialize Guid null``() = 
        do testDeserialize Guid.Empty "null"

    [<Test>] 
    member test.``FsJson: Deserialize Guid format N``() = 
        do testDeserialize (Guid("b2f83697-d509-4c52-8ba9-d6a0961181c5")) "\"b2f83697d5094c528ba9d6a0961181c5\""

    [<Test>] 
    member test.``FsJson: Deserialize Guid format B``() = 
        do testDeserialize (Guid("b2f83697-d509-4c52-8ba9-d6a0961181c5")) "\"{b2f83697-d509-4c52-8ba9-d6a0961181c5}\""

    [<Test>] 
    member test.``FsJson: Deserialize Guid format P``() = 
        do testDeserialize (Guid("b2f83697-d509-4c52-8ba9-d6a0961181c5")) "\"(b2f83697-d509-4c52-8ba9-d6a0961181c5)\""

    [<Test>] 
    member test.``FsJson: Serialization CultureInfo.InvariantCulture``() = 
        do testSerialization CultureInfo.InvariantCulture "\"\""

    [<Test>] 
    member test.``FsJson: Serialization CultureInfo es-ES``() = 
        do testSerialization (CultureInfo.GetCultureInfo("es-ES")) "\"es-ES\""

    [<Test>] 
    member test.``FsJson: Deserialize CultureInfo null``() = 
        let value: CultureInfo = null
        do testDeserialize value "null"

    [<Test>] 
    member test.``FsJson: Serialization List empty``() = 
        let value: string list = []
        do testSerialization value "[]"

    [<Test>] 
    member test.``FsJson: Serialization List of string``() = 
        do testSerialization ["1"; "2"; "3"] """["1","2","3"]"""

    [<Test>] 
    member test.``FsJson: Serialization List of int``() = 
        do testSerialization [1; 2; 3] "[1,2,3]"

    [<Test>] 
    member test.``FsJson: Serialization Set empty``() = 
        let value: Set<string> = [] |> Set.ofList
        do testSerialization value "[]"

    [<Test>] 
    member test.``FsJson: Serialization Set of string``() = 
        do testSerialization (["1"; "2"; "3"] |> Set.ofList) """["1","2","3"]"""

    [<Test>] 
    member test.``FsJson: Serialization Set of int``() = 
        do testSerialization ([1; 2; 3] |> Set.ofList) "[1,2,3]"

    [<Test>] 
    member test.``FsJson: Serialization Map empty``() = 
        let value = Map.empty<string, int>
        do testSerialization value "{}"

    [<Test>] 
    member test.``FsJson: Serialization Map``() = 
        let value = ["1",1; "2",2; "3",3] |> Map.ofList
        do testSerialization value """{"1":1,"2":2,"3":3}"""

    [<Test>] 
    member test.``FsJson: Serialization Uri absolute``() = 
        let value = Uri("http://www.google.com")
        do testSerialization value "\"http://www.google.com\""

    [<Test>] 
    member test.``FsJson: Serialization Uri relative``() = 
        let value = Uri("/path/to/resource?q=123", UriKind.Relative)
        do testSerialization value "\"/path/to/resource?q=123\""

    [<Test>] 
    member test.``FsJson: Serialization Uri null``() = 
        let value: Uri = null
        do testSerialization value "null"

    [<Test>] 
    member test.``FsJson: Serialization Record``() = 
        let text = """{"id":"record-id","amount":3.14159,"anInt":42,"aList":[true,true,false]}"""
        let value = 
            {
                id = "record-id" 
                amount = 3.14159
                anInt = Some 42
                aList = [true; true; false]
            }
        do testSerialization value text

    [<Test>] 
    member test.``FsJson: Serialization Union enum``() = 
        do testSerialization TestEnum.Value2 "2"

    [<Test>] 
    member test.``FsJson: Serialization Union simple``() = 
        do testSerialization Option3 "\"Option3\""

    [<Test>] 
    member test.``FsJson: Serialization Union complex case 1``() = 
        let text = """{"SuccessfulResult":{"errors":["warning1","warning2"],"result":true}}"""
        let value = SuccessfulResult (true, ["warning1"; "warning2"])
        do testSerialization value text

    [<Test>] 
    member test.``FsJson: Serialization Union complex case 2``() = 
        let text = """{"FailedResult":{"errors":["error#1","error#2"]}}"""
        let value = FailedResult ["error#1"; "error#2"]
        do testSerialization value text

    [<Test>] 
    member test.``FsJson: Serialization Union complex case 3``() = 
        let text = """{"DoNothingResult":{}}"""
        let value = DoNothingResult
        do testSerialization value text

    [<Test>] 
    member test.``FsJson: Serialization Complex sample``() = 
        let text = """[42,"a string",["a tuple",1],"b2f83697-d509-4c52-8ba9-d6a0961181c5",{"1":1,"2":2,"3":3},null]"""
        let value = (
            42, Some "a string", 
            ("a tuple", 1), 
            Guid("{b2f83697-d509-4c52-8ba9-d6a0961181c5}"), 
            ["1",1; "2",2; "3",3] |> Map.ofList, 
            None)
        do testSerialization value text

