module Interpreter.Memory

    type memory = { map: Map<int, int>; next: int }

    let empty (memSize: int) : memory = 
        { map = Map.empty; next = 0 }

    let alloc (size: int) (mem: memory) : (memory * int) option = 
        if size > 0 then
            let newMap =
                List.fold (fun acc addr -> Map.add addr 0 acc) mem.map [mem.next .. mem.next + size - 1]
            let newMem = { map = newMap; next = mem.next + size }
            Some (newMem, mem.next) 
        else None

    let free (ptr: int) (size: int) (mem: memory) : memory option = 
        if size > 0 then
            if List.forall (fun addr -> Map.containsKey addr mem.map) [ptr .. ptr + size - 1] then
                let newMap = List.fold (fun acc addr -> Map.remove addr acc) mem.map [ptr .. ptr + size - 1]
                Some { mem with map = newMap }
            else None
        else None

    let setMem (ptr: int) (v: int) (mem: memory) : memory option = 
        if Map.containsKey ptr mem.map then
            let newMap = Map.add ptr v mem.map
            Some { mem with map = newMap }
        else None

    let getMem (ptr: int) (mem: memory) : int option =
        Map.tryFind ptr mem.map
