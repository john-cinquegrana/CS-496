(* This file defines expressed values and environments *)

(* expressed values and environments are defined mutually recursively *)


type exp_val =
  | NumVal of int
  | BoolVal of bool
  | UnitVal       
  | TupleVal of exp_val list               
type env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env


(* Environment Abstracted Result *)

type 'a result = Ok of 'a | Error of string

type 'a ea_result = env -> 'a result
  
let return : 'a -> 'a ea_result = fun v ->
  fun _env ->
  Ok v

let error : string -> 'a ea_result = fun s ->
  fun _env -> 
  Error s

let (>>=) : 'a ea_result -> ('a -> 'b ea_result) -> 'b ea_result = fun c f ->
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok v -> f v env

let (>>+) : env ea_result -> 'a ea_result -> 'a ea_result = fun c d ->
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok newenv -> d newenv

let run : 'a ea_result -> 'a result = fun c ->
  c EmptyEnv

let lookup_env : env ea_result =
  fun env ->
  Ok env

    
(* Operations on environments *)

let empty_env : unit -> env ea_result = fun () ->
  return EmptyEnv

let extend_env : string -> exp_val -> env ea_result = fun id v env ->
  Ok (ExtendEnv(id,v,env))

let rec apply_env : string -> exp_val ea_result = fun id env ->
  match env with
  | EmptyEnv -> Error (id^" not found!")
  | ExtendEnv(v,ev,tail) ->
    if id=v
    then Ok ev
    else apply_env id tail

let rec extend_env_list : string list -> exp_val list -> env ea_result = fun ids evs ->
  match ids,evs  with
  | [],[] -> lookup_env
  | id::ids, ev::evs when List.mem id ids -> error "duplicate identifiers"
  | id::ids, ev::evs -> extend_env id ev >>+ extend_env_list ids evs
  | _,_ -> error "extend_env_list: Arguments do not match parameters!"


(* operations on expressed values *)

let int_of_numVal : exp_val -> int ea_result =  function
  |  NumVal n -> return n
  | _ -> error "Expected a number!"

let bool_of_boolVal : exp_val -> bool ea_result =  function
  |  BoolVal b -> return b
  | _ -> error "Expected a boolean!"

let tuple_of_tupleVal : exp_val -> exp_val list ea_result =  function
  |  TupleVal b -> return b
  | _ -> error "Expected a tuple!"

let rec string_of_list_of_strings = function
  | [] -> ""
  | [id] -> id
  | id::ids -> id ^ "," ^ string_of_list_of_strings ids


let rec string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | UnitVal  -> "UnitVal"
  | TupleVal(evs) ->  "Tuple (" ^ string_of_list_of_strings (List.map
                                                   string_of_expval
                                                   evs)  ^ ")"  
                      
let rec string_of_env'  = function
  | EmptyEnv -> ""
  | ExtendEnv(id,v,env) -> string_of_env' env^" ("^id^","^string_of_expval v^")\n"

let string_of_env : string ea_result = fun env ->
  Ok ("Environment:\n"^ string_of_env' env)

let rec has_error : 'a result list -> bool = fun ls ->
  match ls with
  | h::t -> begin
    match h with
    | Error err -> true
    | Ok s -> has_error t
    end
  | [] -> false

let rec get_error : 'a result list -> 'a result = fun ls ->
  match ls with
  | h::t -> begin
    match h with
    | Error err -> Error err
    | Ok s -> get_error t
    end
  | [] -> failwith "No error found"

let val_of_result : 'a result -> 'a = fun arg ->
  match arg with
  | Ok s -> s
  | Error err -> failwith "Bad call to val_of_result"

let test_list : 'a list -> 'b list -> 'c ea_result = fun ls1 ls2 ->
  fun env ->
  if ( List.length ls1 != List.length ls2 ) then
    Error "extend_env_list: No.of args does not match no. of params!"
  else Ok UnitVal