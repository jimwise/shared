(* we use lists instead of the Stack stdlib, as we need a functional structure *)

type op = {
    doc : string ;
    action : float list -> float list
  } ;;

let dict = Hashtbl.create 32 ;;

let keys d = Hashtbl.fold (fun k v acc -> k :: acc) d [] ;;

let make_op name d a =
  Hashtbl.add dict name { doc=d; action=a } ;;

let signal_error s =
  print_endline s ;;

let underflow stack =
  signal_error "stack underflow"; stack ;;

let show_help () =
  Printf.printf "%d Commands:\n" ( Hashtbl.length dict ) ;
  let keys = List.sort compare (keys dict) in
  let print_cmd k = Printf.printf "%s -- %s\n" k (Hashtbl.find dict k).doc in
  List.iter print_cmd keys ;;

make_op "." "display top value on the stack" (
function ( x :: stack ) -> Printf.printf "%f\n" x ; x :: stack 
  | stack -> underflow stack )
;;
make_op "#" "display number of values on the stack" ( 
function stack -> print_int ( List.length stack ); print_newline () ; stack )
;;

make_op "+" "replace top two values on the stack with their sum" (
function ( y :: x :: stack ) -> ( x +. y ) :: stack
  | stack -> underflow stack )
;;
make_op "-" "replace top two values on the stack with their difference" (
function ( y :: x :: stack ) -> ( x -. y ) :: stack
  | stack -> underflow stack ) 
;;
make_op "*" "replace top two values on the stack with their product" (
function ( y :: x :: stack ) -> ( x *. y ) :: stack
  | stack -> underflow stack )
;;
make_op "/" "replace top two values on the stack with their quotient" (
function ( y :: x :: stack ) -> ( x /. y ) :: stack
  | stack -> underflow stack )
;;
make_op "^" "replace top two values on the stack, x and y, with x to the yth power" (
function ( y :: x :: stack ) -> ( x ** y ) :: stack
  | stack -> underflow stack )
;;

make_op "drop" "remove top value from the stack" (
function ( x :: stack ) -> stack
  | stack -> underflow stack )
;;
make_op "dup" "duplicate top value on the stack" (
function ( x :: stack ) -> x :: x :: stack
  | stack -> underflow stack )
;;
make_op "swap" "swap top two values on the stack" (
function ( y :: x :: stack ) -> x :: y :: stack
  | stack -> underflow stack )
;;

make_op "help" "display this help" ( function stack -> show_help () ; stack ) ;;


let act stack s =
  try
    (Hashtbl.find dict s).action stack ;
  with
    Not_found ->
      try
        ( float_of_string s ) :: stack ;
      with
        Failure _ -> signal_error ("unknown operation: " ^ s) ; stack
;;
  
let rec rpn stack =
  try
    print_string "> " ;
    flush stdout ;
    let s = read_line() in
    let newstack = act stack s in
    rpn newstack
  with
    End_of_file ->
      print_endline "" ;; (* XXX not sure why print_newline doesn't do it *)
  
rpn [ ];;
