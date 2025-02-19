module Assignment2

let rec downto1 (n: int) =
    if n <= 0 then [] else n :: downto1 (n - 1)

let rec downto2 (n: int) =
    match n with
    | 0 -> []
    | _ -> n :: downto2 (n - 1)

let rec downto3 =
    function
    | n when n <= 0 -> []
    | n -> n :: downto3 (n - 1)

let removeOddIdx (xs: 'a list) : 'a list =
    let rec helper (lst: 'a list) (index: int) =
        match lst with
        | [] -> []
        | x :: xs ->
            if index % 2 = 0 then
                x :: helper xs (index + 1)
            else
                helper xs (index + 1)

    helper xs 0


let combinePair (xs: 'a list) =
    let rec helper (lst: 'a list) =
        match lst with
        | x :: y :: xs -> (x, y) :: helper xs
        | _ -> []

    helper xs


type complex = float * float // Fill in your type here
let mkComplex (a: float) (b: float) : complex = (a, b)

let complexToPair (c: complex) : float * float = (fst c, snd c)

let (|+|) (c1: complex) (c2: complex) : complex = (fst c1 + fst c2, snd c1 + snd c2)

let (|*|) (c1: complex) (c2: complex) : complex =
    (fst c1 * fst c2 - snd c1 * snd c2, snd c1 * fst c2 + fst c1 * snd c2)

let (|-|) (c1: complex) (c2: complex) = (fst c1 - fst c2, snd c1 - snd c2)

let (|/|) (c1: complex) (c2: complex) =
    let (a, b) = c1
    let (c, d) = c2
    let denom = c * c + d * d
    ((a * c + b * d) / denom, (b * c - a * d) / denom)

let explode1 (s: string) : char list = s |> Seq.toList

let rec explode2 (s: string) : char list =
    match s with
    | "" -> []
    | s -> s.[0] :: explode2 s.[1..]

let rec implode (cs: char list) : string =
    match cs with
    | [] -> ""
    | c :: lst -> string c + implode lst

let rec implodeRev (cs: char list) : string =
    match cs with
    | [] -> ""
    | c :: lst -> implodeRev lst + string c

let toUpper (s: string) : string =
    s |> explode2 |> List.map System.Char.ToUpper |> implode

let toUpper2: string -> string = explode2 >> List.map System.Char.ToUpper >> implode

let rec ack (m, n) : int =
    match m, n with
    | 0, n -> n + 1
    | m, 0 when m > 0 -> ack (m - 1, 1)
    | m, n when m > 0 && n > 0 -> ack (m - 1, ack (m, n - 1))
