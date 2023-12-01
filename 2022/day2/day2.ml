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

let transform_choice choice =
  if choice = "A" || choice = "X" then 0 
  else if choice = "B" || choice = "Y" then 1
  else 2

let split_string round = round
  |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> List.map transform_choice;;

type round = {he : int; me : int;};;
let rounds = read_file "strategy.txt" 
  |> List.map split_string 
  |> List.map (fun h -> {he = List.hd h; me = List.nth h 1});;

let amount_choice choice rounds = 
  (List.filter (fun x -> x.me = choice) rounds |> List.length) * (choice + 1)

let first_score round_list = 
  amount_choice 0 round_list + amount_choice 1 round_list + amount_choice 2 round_list

let transform_score score =
  if score = 1 then 0
  else if score = 0 then 3
  else 6

let score_winning (round: round) = (3 + round.he - round.me) mod 3
  |> transform_score;;
let my_win_score = List.fold_left (+) 0 (List.map score_winning rounds)

let task1 = first_score rounds + my_win_score;;

let adjust_column round = 
  (** tie **)
  if round.me = 1 then {he=round.he; me=round.he}
  (** need to lose **)
  else if round.me = 0 then {he=round.he; me=((round.he + 2) mod 3)}  
    (** need to win **)
  else if round.me = 2 then {he=round.he; me=((round.he + 1) mod 3)} 
  else round

let adjusted_rounds = List.map adjust_column rounds
let adjusted_my_win_score = List.fold_left (+) 0 (List.map score_winning adjusted_rounds)

let task2 = first_score adjusted_rounds + adjusted_my_win_score;;
print_int task2
