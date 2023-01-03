open System.IO 
let input = File.ReadAllLines "/tmp/aoc/input" |> Seq.toList 

let snafu2i (s:string) : int64 =
    let s = s.ToCharArray () |> Array.rev |> Array.toList
    let rec snafu2i (s:char list) =
        match s with
        | '0'::rest -> snafu2i rest * 5L
        | '1'::rest -> snafu2i rest * 5L + 1L
        | '2'::rest -> snafu2i rest * 5L + 2L
        | '-'::rest -> snafu2i rest * 5L - 1L
        | '='::rest -> snafu2i rest * 5L - 2L
        | [] -> 0L
    snafu2i s

let i2snafu (i:int64) : string =
    let rec i2snafu (i:int64) : string list = 
        if i = 0l then []
        else
            let high : int64 = i / 5L 
            let curr = i % 5L
            let high = if curr > 2l then high + 1L else high
            let curr = if curr > 2l then curr - 5L else curr
            let c =
                match curr with
                | 0L -> "0"
                | 1L -> "1"
                | 2L -> "2"
                | -1L -> "-"
                | -2L -> "="
            c :: (i2snafu high)
    i2snafu i
    |> List.rev
    |> String.concat ""
        
let task1 () =
    input |> List.map snafu2i |> List.sum |> i2snafu |> printfn "Task 1 : %s"
    
task1 ()