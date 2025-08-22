module Interpreter.Eval

    open Result
    open Language
    open State

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

        
        
    let rec arithEval (expr: aexpr) (st: state) : int option =
        let eval = fun e -> arithEval e st

        let add = fun a b -> a |> Option.bind (fun x -> b |> Option.map (fun y -> x + y))
        let mul = fun a b -> a |> Option.bind (fun x -> b |> Option.map (fun y -> x * y))
        let divide = fun a b -> a |> Option.bind (fun x -> b |> Option.bind (fun y -> if y <> 0 then Some (x / y) else None))
        let modulo = fun a b -> a |> Option.bind (fun x -> b |> Option.bind (fun y -> if y <> 0 then Some (x % y) else None))

        match expr with
        | Num x -> Some x
        | Var v -> Map.tryFind v st.var
        | Add (b, c) -> add (eval b) (eval c)
        | Mul (b, c) -> mul (eval b) (eval c)
        | Div (b, c) -> divide (eval b) (eval c)
        | Mod (b, c) -> modulo (eval b) (eval c)
        | MemRead e1 -> Option.bind (fun ptr -> getMem ptr st) (arithEval e1 st)
        | Random -> Some (State.random st)
        | Read -> Some (readInt())
        | Cond(b, a1, a2) -> 
            boolEval b st |> 
            Option.bind (fun res -> if res then arithEval a1 st else arithEval a2 st)
        | _ -> Some 0
    and boolEval (b: bexpr) (st: state) : bool option =
        match b with
        | TT -> Some true
        | Eq (a, c) ->
            match arithEval a st, arithEval c st with
            | Some x, Some y -> Some (x = y)
            | _ -> None
        | Lt (a, c) ->
            match arithEval a st, arithEval c st with
            | Some x, Some y -> Some (x < y)
            | _ -> None
        | Conj (a, c) ->
            match boolEval a st, boolEval c st with
            | Some x, Some y -> Some (x && y)
            | _ -> None
        | Not a ->
            match boolEval a st with
            | Some x -> Some (not x)
            | None -> None

    let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList

    let mergeStrings (es: aexpr list) (s: string) (st: state) : string option = 
        let rec helper (es: aexpr list) (s: string list) (acc : string) : string option =
            match es, s with
            | [], [h] -> Some (acc + h)
            | headex :: tailex, heads :: tails -> 
                match arithEval headex st with
                | Some y -> helper tailex tails (acc + heads + string y)
                | None -> None
            | _, _ -> None
        helper es (split s "%") ""
    
    let mergeStrings2 (es: aexpr list) (s: string) (st: state) : string option =
        let rec helper (es: aexpr list) (s: string list) c : string option =
            match es, s with
            | [], [h] -> Some (c h)
            | headex :: tailex, heads :: tails -> 
                match arithEval headex st with
                | Some y -> helper tailex tails (fun s -> c (heads + (string) y + s))
                | None -> None
            | _, _ -> None
        helper es (split s "%") id

    let rec stmntEval (s: stmnt) (st: state) : state option =
        match s with
        | Skip -> Some st
        | Declare v -> 
            if Map.containsKey v st.var then None 
            else Some { st with var = Map.add v 0 st.var }
        | Assign (v, a) ->
            match arithEval a st with
            | Some x -> Some { st with var = Map.add v x st.var }
            | None -> None
        | Seq (s1, s2) ->
            match stmntEval s1 st with
            | Some st' -> stmntEval s2 st'
            | None -> None
        | If (guard, s1, s2) ->
            match boolEval guard st with
            | Some true -> stmntEval s1 st
            | Some false -> stmntEval s2 st
            | None -> None
        | While (guard, s) -> 
            let rec loop st =
                match boolEval guard st with
                | Some true ->
                    match stmntEval s st with
                    | Some st' -> loop st'
                    | None -> None
                | Some false -> Some st
                | None -> None
            loop st
        | Alloc (x, e) ->
            if Map.containsKey x st.var then
                arithEval e st
                |> Option.bind (fun size -> 
                    State.alloc x size st) 
            else None
        | Free (e1, e2) ->
            match arithEval e1 st, arithEval e2 st with
            | Some ptr, Some size ->
                State.free ptr size st
            | _ -> None
        | MemWrite(e1, e2) ->
            match arithEval e1 st, arithEval e2 st with
            | Some ptr, Some v ->
                State.setMem ptr v st
            | _ -> None
        | Print(es, s) -> 
            Option.bind(fun s -> 
                printfn "%A" s 
                Some st )(mergeStrings2 es s st)
        | _ -> Some st

   