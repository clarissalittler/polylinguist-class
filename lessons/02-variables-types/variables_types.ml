(* variables_types.ml *)

(* Type inference - no annotations needed *)
let age = 25                    (* inferred as int *)
let price = 19.99               (* inferred as float *)
let name = "Alice"              (* inferred as string *)
let is_student = true           (* inferred as bool *)

(* Optional type annotations *)
let (explicit_age : int) = 25
let (explicit_name : string) = "Alice"

(* Immutable by default *)
(* let age = 30  (* ERROR: cannot reassign *) *)

(* Mutable references (if needed) *)
let counter = ref 0             (* int ref *)
let () = counter := !counter + 1  (* dereference and assign *)

(* Tuples *)
let person = ("Alice", 25)      (* string * int *)

(* Lists (same type) *)
let numbers = [1; 2; 3; 4; 5]   (* int list *)

(* Printing *)
let () = Printf.printf "Age: %d\n" age
let () = Printf.printf "Name: %s\n" name
let () = Printf.printf "Price: %.2f\n" price
let () = Printf.printf "Is student: %b\n" is_student
let () = Printf.printf "Counter: %d\n" !counter
