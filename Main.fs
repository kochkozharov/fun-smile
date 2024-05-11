open Interpreter
open Parser
open System.IO
open Preprocessor

let readTextFromFile (filePath: string) =
    try
        processFile(filePath)
    with
    | :? System.IO.FileNotFoundException ->
        failwithf "File not found: %s" filePath
    | ex ->
        failwithf "Error reading file: %s" (ex.ToString())

[<EntryPoint>]
let main argv =
    let filePath = argv.[0]
    let fileContent = readTextFromFile filePath
    printExpr (exec (getResult fprogram fileContent ))
    0 
