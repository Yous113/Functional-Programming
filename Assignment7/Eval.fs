module Interpreter.Eval

    open Language
    open StateMonad

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

    let rec arithEval (expr: aexpr) : int stateMonad =
        match expr with
        | Num x -> ret x

        | Var v -> getVar v

        | Add (e1, e2) ->
            arithEval e1 >>= fun x ->
            arithEval e2 >>= fun y ->
            ret (x + y)

        | Mul (e1, e2) ->
            arithEval e1 >>= fun x ->
            arithEval e2 >>= fun y ->
            ret (x * y)

        | Div (e1, e2) ->
            arithEval e1 >>= fun x ->
            arithEval e2 >>= fun y ->
            if y = 0 then fail else ret (x / y)

        | Mod (e1, e2) ->
            arithEval e1 >>= fun x ->
            arithEval e2 >>= fun y ->
            if y = 0 then fail else ret (x % y)

        | MemRead e1 ->
            arithEval e1 >>= getMem

        | Random -> random

        | Read -> ret (readInt())

        | Cond (b, a1, a2) ->
            boolEval b >>= fun cond ->
            if cond then arithEval a1 else arithEval a2
        | _ -> ret 0

    and boolEval (b: bexpr) : bool stateMonad =
        match b with
        | TT -> ret true

        | Eq (a1, a2) ->
            arithEval a1 >>= fun x ->
            arithEval a2 >>= fun y ->
            ret (x = y)

        | Lt (a1, a2) ->
            arithEval a1 >>= fun x ->
            arithEval a2 >>= fun y ->
            ret (x < y)

        | Conj (b1, b2) ->
            boolEval b1 >>= fun x ->
            boolEval b2 >>= fun y ->
            ret (x && y)

        | Not b1 ->
            boolEval b1 >>= fun x ->
            ret (not x)

    type StateBuilder() =  
        member this.Bind(f, x) = (>>=) f x  
        member this.Return(x) = ret x  
        member this.ReturnFrom(x) = x  
        member this.Combine(a, b) = a >>= (fun _ -> b) 
      
    let eval = StateBuilder()

    let rec arithEval2 (expr: aexpr) : int stateMonad =
        eval {
            match expr with
            | Num x -> return x

            | Var v -> return! getVar v

            | Add (a, b) ->
                let! x = arithEval2 a
                let! y = arithEval2 b
                return x + y

            | Mul (a, b) ->
                let! x = arithEval2 a
                let! y = arithEval2 b
                return x * y

            | Div (a, b) ->
                let! x = arithEval2 a
                let! y = arithEval2 b
                if y = 0 then return! fail
                else return x / y

            | Mod (a, b) ->
                let! x = arithEval2 a
                let! y = arithEval2 b
                if y = 0 then return! fail
                else return (x % y)

            | MemRead e ->
                let! ptr = arithEval2 e
                return! getMem ptr

            | Random -> return! random

            | Read -> return readInt ()

            | Cond (b, a1, a2) ->
                let! cond = boolEval2 b
                if cond then return! arithEval2 a1
                else return! arithEval2 a2
            | _ -> return 0
        }

    and boolEval2 (b: bexpr) : bool stateMonad =
        eval {
            match b with
            | TT -> return true

            | Eq (a, b) ->
                let! x = arithEval2 a
                let! y = arithEval2 b
                return x = y

            | Lt (a, b) ->
                let! x = arithEval2 a
                let! y = arithEval2 b
                return x < y

            | Conj (a, b) ->
                let! x = boolEval2 a
                let! y = boolEval2 b
                return x && y

            | Not a ->
                let! x = boolEval2 a
                return not x
        }
    let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList

    let mergeStrings2 (es: aexpr list) (s: string) : string stateMonad =
        let rec helper (es: aexpr list) (s: string list) (c: string -> string) : string stateMonad =
            match es, s with
            | [], [h] ->
                ret (c h)

            | headex :: tailex, heads :: tails ->
                eval {
                    let! y = arithEval2 headex
                    return! helper tailex tails (fun s -> c (heads + string y + s))
                }

            | _, _ ->
                fail

        helper es (split "%" s) id

    let rec stmntEval (s: stmnt) : unit stateMonad =
        eval {
            match s with
            | Skip ->
                return ()

            | Declare v ->
                return! declare v

            | Assign (v, a) ->
                let! value = arithEval2 a
                return! setVar v value

            | Seq (s1, s2) ->
                do! stmntEval s1
                return! stmntEval s2

            | If (guard, s1, s2) ->
                let! cond = boolEval2 guard
                if cond then return! stmntEval s1
                else return! stmntEval s2

            | While (guard, body) ->
                let rec loop () = eval {
                    let! cond = boolEval2 guard
                    if cond then
                        do! stmntEval body
                        return! loop ()
                    else
                        return ()
                }
                return! loop ()

            | Alloc (x, e) ->
                let! size = arithEval2 e
                return! alloc x size

            | Free (e1, e2) ->
                let! ptr = arithEval2 e1
                let! size = arithEval2 e2
                return! free ptr size

            | MemWrite (e1, e2) ->
                let! ptr = arithEval2 e1
                let! value = arithEval2 e2
                return! setMem ptr value

            | Print (es, s) ->
                let! str = mergeStrings2 es s
                printfn "%s" str
                return ()
            | _ -> return ()
        }
