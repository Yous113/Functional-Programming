
module Assignment1

    open System

    let sqr (x: int) : int =
        x * x

    let pow (x: float) (n: float) : float =
        System.Math.Pow(x, n)

    let rec fib (n: int) : int =
        match n with
        | 0 -> 0
        | 1 -> 1
        | _ -> fib (n - 1) + fib (n - 2)

    let rec sum (n: int) : int =
        match n with
        | 0 -> 0
        | _ -> n + sum (n - 1)

    let dup (s: string) : string =
        s + s

    let rec dupn (s: string) (n: int) : string =
        if n <= 0 then
            ""
        else
            s + dupn s (n - 1)

    let readFromConsole () : string =
        System.Console.ReadLine().Trim()

    let tryParseInt (str: string) : bool * int =
        System.Int32.TryParse str

    let rec readInt () : int =
        let input = readFromConsole ()
        let (success, result) = tryParseInt input

        if success then
            printfn "%d" result
            result
        else
            printfn "%s is not an integer" input
            readInt ()

    let rec bin (n: int, k: int) : int =
        if k = 0 || k = n then
            1
        else
            bin (n - 1, k - 1) + bin (n - 1, k)


    let timediff _ = failwith "not implemented"
    let minutes _ = failwith "not implemented"
    let curry _ = failwith "not implemented"
    let uncurry _ = failwith "not implemented"
