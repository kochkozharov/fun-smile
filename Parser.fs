// For more information see https://aka.ms/fsharp-console-apps
module Parser
open FParsec
open Constants

let str s = pstring s .>> spaces

