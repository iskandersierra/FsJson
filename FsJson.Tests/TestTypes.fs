module FsJson.Tests.TestTypes

open System

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Converters

type TestRecord = 
    {
        id: string
        amount: double
        anInt: int option
        aList: bool list
    }

type TestSingleOption = 
    | TestSingleOption of title: string * distance: int

type TestEnum =
    | Value1 = 1
    | Value2 = 2
    | Value3 = 3
    | Value4 = 4

type TestSimpleOptions =
    | Option1
    | Option2
    | Option3
    | Option4

type TestComplexOptions<'result, 'error> =
    | SuccessfulResult of result: 'result * errors: 'error list
    | FailedResult of errors: 'error list
    | DoNothingResult

type TestTuple = string * int * DateTime option * string list 
