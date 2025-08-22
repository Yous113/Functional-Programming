module Interpreter.State

    open Result
    open Language
    open Memory
    
    let validVariableName (v: string) : bool = 
        match v with
        | v when System.Char.IsAsciiLetter v.[0] || v.[0] = '_' -> 
            String.forall (fun c -> System.Char.IsAsciiLetterOrDigit(c) || c = '_') v
        | _ -> false

    let reservedVariableName (v: string) : bool = 
        let reserved : string list = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]
        List.exists (fun s -> s = v) reserved
        
    
    type state = { var: Map<string, int>; mem: memory; rng: System.Random}
    
    let mkState (memSize : int) (oseed : int option) ( program : program) : 
        state = { var = Map.empty; 
            mem = empty memSize; 
            rng = 
                match oseed with
                | Some seed -> System.Random seed
                | None -> System.Random()
                }
        

    let random st : int = st.rng.Next()

    
    let declare (x : string) (st : state) : state option = 
        if Map.containsKey x st.var then
            None
        elif reservedVariableName x then
            None
        elif not (validVariableName x) then
            None
        else
            Some { st with var = Map.add x 0 st.var }
        
    
    let getVar (x: string) (st: state) : int option =
        match Map.tryFind x st.var with
        | Some value -> Some value  
        | None -> None 

    let setVar (x: string) (value: int) (st: state) : state option =
        match Map.tryFind x st.var with
        | Some _ -> Some { st with var = Map.add x value st.var }  
        | None -> None

    let alloc (x : string) (size : int) (st : state) : state option =
        Memory.alloc size st.mem |> Option.bind(fun (mem', next) -> setVar x next {st with mem = mem'})

    let free (ptr: int) (size: int) (st: state) : state option =
        Memory.free ptr size st.mem |> Option.bind (fun newmem -> Some {st with mem = newmem})

    let getMem (ptr : int) (st: state) : int option =
        Memory.getMem ptr st.mem
    
    let setMem (ptr: int) (v: int) (st: state) : state option =
        Memory.setMem ptr v st.mem |> Option.bind (fun newmem -> Some {st with mem = newmem})



    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     

    let pushFrame _ = failwith "not implemented"
    let popFrame _ = failwith "not implemented"