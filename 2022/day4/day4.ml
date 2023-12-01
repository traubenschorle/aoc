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

let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j [] ;;

let create_range (range: int list) = (List.hd range)--(List.nth range 1)
let subset (a: int list) (b: int list) = 
  let module SS = Set.Make (Int) in
  let a_set = SS.of_list a in 
  let b_set = SS.of_list b in 
  (SS.subset a_set b_set) || (SS.subset b_set a_set)

let have_intersection (a: int list) (b: int list) = 
  let module SS = Set.Make (Int) in
  List.length (SS.elements (SS.inter (SS.of_list a) (SS.of_list b))) > 0

(**
data structure:
[
  [[2, 3, 4], [6, 7, 8]],
  [[2, 3], [4, 5]],
  [[5, 6, 7], [7, 8, 9]],
  ...
]
**)
let data = read_file "day4_input.txt"
  |> List.map (String.split_on_char ',')
  |> List.map (List.map (String.split_on_char '-'))
  |> List.map (List.map (List.map int_of_string))
  |> List.map (List.map (create_range))

let task1 = data
  |> List.map (fun pairs -> subset (List.hd pairs) (List.nth pairs 1)) 
  |> List.map (fun subset_bool -> if subset_bool then 1 else 0)
  |> List.fold_left (+) 0;;

let task2 = data 
  |> List.map (fun pairs -> have_intersection (List.hd pairs) (List.nth pairs 1)) 
  |> List.map (fun intersection_bool -> if intersection_bool then 1 else 0)
  |> List.fold_left (+) 0;;
