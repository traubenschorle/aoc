let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let rec parse_input input = 
  match input with 
  | [] -> []
  | "" :: t -> 0 :: parse_input t 
  | h :: t -> int_of_string h :: parse_input t;;

let rec build_sum_list (counter : int) (calorie_list: int list) = 
  match calorie_list with
  | [] -> []
  | [x] -> [x]
  | 0 :: t -> counter :: build_sum_list 0 t
  | h :: t -> build_sum_list (counter + h) t;; 

let sum_list = read_file "input.txt" 
  |> parse_input 
  |> build_sum_list 0 ;;

let task1 = List.fold_left max 0 sum_list;;
print_int task1;;  

let rec firstk k xs = match xs with
  | [] -> failwith "firstk"
  | x::xs -> if k = 1 then [x] else x::firstk (k - 1) xs;;

let task2 = List.sort (Fun.flip compare) sum_list 
  |> firstk 3 
  |> List.fold_left (+) 0;;
(** print_int task2 *) 