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

let input = read_file "day3_data.txt"
let convert_string_to_list s =  s |> String.to_seq |> List.of_seq
let rucksacks = List.map convert_string_to_list input
let split_point rucksack = List.length (rucksack) / 2;;

let rec firstk k xs = match xs with
  | [] -> failwith "firstk"
  | x::xs -> if k = 1 then [x] else x::firstk (k - 1) xs;;

let wrong_item rucksack = 
  let first = firstk (split_point rucksack) rucksack in
  let second = List.rev (firstk (split_point rucksack) (List.rev rucksack)) in 
  List.filter (fun x -> List.mem x first) second 
  |> List.hd;; 

let wrong_items = List.map (fun a -> wrong_item a) rucksacks;;

let priority = function
  | 'a' .. 'z' as c -> Char.code c - Char.code 'a' + 1
  | 'A' .. 'Z' as c -> Char.code c - Char.code 'A' + 27
  | _ -> assert false

let task1 = List.fold_left (+) 0 (List.map (fun a -> priority a) wrong_items);;

let intersect e1 e2 = List.filter (fun x -> List.mem x e1) e2
let test = List.hd (intersect (intersect (List.nth rucksacks 0) (List.nth rucksacks 1)) (List.nth rucksacks 2));;

let solve_group a b c = List.hd (intersect (intersect a b) c) 

let rec task2_solution rucksack_list = 
  match rucksack_list with 
  | a :: b :: c :: rest -> solve_group a b c :: task2_solution rest 
  | [] -> []
  | _ -> assert false;;

let task2 = List.fold_left (+) 0 (List.map (fun a -> priority a) (task2_solution rucksacks));;
print_int task2
