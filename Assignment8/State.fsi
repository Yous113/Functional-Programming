module Interpreter.State

    open Memory
    open Language
    
    type state = { var: Map<string, int>; mem: memory; rng: System.Random}

    val mkState: int -> int option -> program -> state

    val random: state -> int

    val declare: string -> state -> state option
    val getVar: string -> state -> int option
    val setVar: string -> int -> state -> state option

    val alloc: string -> int -> state -> state option
    val free: int -> int -> state -> state option
    val getMem: int -> state -> int option
    val setMem: int -> int -> state -> state option

