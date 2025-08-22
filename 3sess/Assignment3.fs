module Assignment3

let add5 (x: int) : int = x + 5;;
let mul3 (x: int) : int = x * 3;;

let add5mul3 (x: int) : int = x |> add5 |> mul3;;
let add5mul3_2: int -> int = add5 >> mul3;;

let add5_2 (f: 'a -> int) (x: 'a) : int = f x |> add5;;
let mul3_2 (f: 'a -> int) (x: 'a) : int = f x |> mul3;;

let rec downto4 (f: int -> 'b -> 'b) (n: int) (e: 'b) : 'b =
    match n with
    | n when n <= 0 -> e
    | _ -> f n (downto4 f (n - 1) e);;

let fac (x: int) : int =
    match x with
    | x when x <= 0 -> 1
    | x -> downto4 (fun x acc -> x * acc) x 1;;


let range (g: int -> 'a) (n: int) : 'a list =
    match n with
    | n when n <= 0 -> []
    | n -> List.rev (downto4 (fun x acc -> g x :: acc) n []);;



let rec double (lst: int list) : int list =
    match lst with
    | [] -> []
    | h :: t -> (h * 2) :: double t;;

let double_2 (lst: int list) : int list = List.map (fun x -> x * 2) lst;;


let rec stringLength (lst: string list) : int list =
    match lst with
    | [] -> []
    | h :: t -> h.Length :: stringLength t;;

let stringLength_2 (lst: string list) : int list =
    List.map (fun (x: string) -> x.Length) lst;;

let rec keepEven (lst: int list) : int list =
    match lst with
    | [] -> []
    | h :: t when h % 2 = 0 -> h :: keepEven t
    | _ :: t -> keepEven t;;

let keepEven_2 (lst: int list) : int list = List.filter (fun x -> x % 2 = 0) lst;;

let rec keepLengthGT5 (lst: string list) : string list =
    match lst with
    | [] -> []
    | h :: t when h.Length > 5 -> h :: keepLengthGT5 t
    | _ :: t -> keepLengthGT5 t;;

let keepLengthGT5_2 (lst: string list) : string list = List.filter (fun x -> x.Length > 5) lst;;

let rec sumPositive (lst: int list) : int =
    match lst with
    | [] -> 0
    | h :: t when h > 0 -> h + sumPositive t
    | _ :: t -> sumPositive t;;

let sumPositive_2 (lst: int list) : int =
    lst |> List.filter (fun x -> x > 0) |> List.sum;;

let sumPositive_3 =
    List.filter (fun x -> x > 0) >> List.fold (fun acc x -> x + acc) 0;;


let add5mul3_3 (f: 'a -> int) (x: 'a) : int =
    x |> add5_2 f |> mul3_2 id;;


let rec mergeFuns (fs: ('a -> 'a) list) : 'a -> 'a = 
    fs |> List.fold (>>) id

let rec facFuns _ = failwith "not implemented"

let fac_2 _ = failwith "not implemented"

let removeOddIdx _ = failwith "not implemented"


let weird _ = failwith "not implemented"


let insert _ = failwith "not implemented"

let rec permutations _ = failwith "not implemented"
