module Synthesis

let abelar i =
    (i > 12) && (i < 3097) &&  i % 12 = 0

let area b h =
    match (b < 0.0 || h < 0.0)  with
    | true -> failwith "Both base and height must be positive."
    | _ -> 0.5 * b * h;

let zollo i =
    match (i < 0) with
    | true -> i * -1
    | _ -> i * 2;

let min a b =
    match a < b with
    | true -> a
    | _ -> b

let max a b =
    match a > b with
    | true -> a
    | _ -> b

let ofTime h m s =
    h * 3600 + m * 60 + s

let toTime s =
    let h = s / 3600
    let m = (s % 3600) / 60
    let secs = (s % 3600) % 60
    match s < 0 with
    | true -> (0, 0, 0)
    | _ -> (h, m, secs)

let digits i =
    let rec digits i acc =
        match i with
        | 0 -> acc
        | n -> digits (n / 10) (1 + acc)
    match i with
    | 0 -> 1
    | _ -> digits i 0


let minmax (a, b, c, d) =
    let max = max (max a b) (max c d)
    let min = min (min a b) (min c d)
    (min, max)

let isLeap y =
    match y < 1582 with
    | true -> failwith "The year must be greater that 1582"
    | _ -> match y % 4 = 0 with
           | true -> (y % 100 <> 0) || (y % 400 = 0)
           | _ -> false

let month = function
    | 1 -> ("January", 31)
    | 2 -> ("February", 28)
    | 3 -> ("March", 31)
    | 4 -> ("April", 30)
    | 5 -> ("May", 31)
    | 6 -> ("June", 30)
    | 7 -> ("July", 31)
    | 8 -> ("August", 31)
    | 9 -> ("September", 30)
    | 10 -> ("October", 31)
    | 11 -> ("November", 30)
    | 12 -> ("December", 31)
    | _ -> failwith "input can only be 1 <= input <= 12."
    

let toBinary i =
    let toBinary = function | 0 -> "0" | _ -> "1"
    let rec toBinaryAll i = 
        match digits i with
        | 1 -> toBinary i
        | _ -> toBinaryAll (i / 10) + toBinary (i % 10)

    match i < 0 with
    | true -> failwith "This function only accepts positive numbers."
    | _ -> toBinaryAll i

let bizFuzz n =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"