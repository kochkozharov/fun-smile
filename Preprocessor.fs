module Preprocessor

open System.IO

let rec processFile (filePath:string) =
    let lines = File.ReadAllLines(filePath)
    let processedLines =
        lines
        |> Array.map (fun line ->
            if line.StartsWith("#include") then
                let includeFile = line.Split(' ').[1]
                processFile(includeFile)
            else
                line
        )
    String.concat "\n" processedLines