open Interpreter
open Parser
open System.IO

let readTextFromFile (filePath: string) =
    try
        File.ReadAllText(filePath)
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
