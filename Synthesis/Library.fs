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
        match i with
        | 0 -> ""
        | _ ->  toBinaryAll (i / 2) + toBinary (i % 2)

    match i < 0 with
    | true -> failwith "This function only accepts positive numbers."
    | _ -> match i = 0 with
           | true -> "0"
           | false -> toBinaryAll i

let bizFuzz n =
    let checkDiv i =
        match i % 3 = 0 && i % 5 = 0 with
        | true -> (1, 1, 1)
        | false -> match (i % 3 = 0, i % 5 = 0)  with
                   | true, false -> (1, 0, 0)
                   | false, true -> (0, 1, 0)
                   | _ -> (0, 0, 0)

    let rec bizFuzz i n acc =
        match i > n with
        | true -> acc
        | _ -> match acc, checkDiv i with
               | (a, b, c), (d, e,f) -> bizFuzz (i + 1) n (a + d, b + e, c + f)
    
    bizFuzz 1 n (0, 0, 0)

let monthDay d y =
    let leap = isLeap y
    let rec getDay i totalDays =
        let delta = match leap && i = 2 with | true -> 1 | _ -> 0
        match d <= totalDays + delta  with
        | true -> match month i with
                    | (mon, _) -> mon
        | false -> match month (i + 1) with
                    | (_, days) -> getDay (i + 1) (totalDays + days + delta)
    match leap with
    | true -> match 1 <= d && d <= 366 with
                | false -> failwith ""
                | true -> getDay 1 31
    | false ->match 1 <= d && d <= 365 with
                | false -> failwith ""
                | true -> getDay 1 31

let coord (x,y) =
    let sqrt n =
        let rec calc i guess =
            match i with
            | 15 -> guess
            | _ ->
                let g = (guess + n / guess) / 2.0
                calc (i + 1) g
        match n <= 0.0 with
        | true -> failwith "Only square roots of positive numbers may be taken."
        | false -> calc 0 (n / 2.0)
            
    let dist (x1, y1) =
        sqrt (((x - x1) * (x - x1)) + ((y - y1) * (y - y1)))
    let containsInitial (x1, y1) w h =
        match x - x1 >= 0.0 &&  x - x1 <= w, y1 - y >= 0.0 &&  y1 - y <= h with
        | true, true -> true
        | _ -> false
    
    dist, containsInitial