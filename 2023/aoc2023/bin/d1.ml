open Str

(* let t1 = D1_input.sample_1 *)
(* let t2 = D1_input.sample_2 *)
let d01_1 = D1_input.in_day_01_01

let find_all_indices text patterns =
  let rec find_from pattern pos acc =
    if pos >= String.length text then acc
    else
      match try Some (search_forward (regexp_string pattern) text pos) with Not_found -> None with
      | Some idx -> find_from pattern (idx + 1) (idx :: acc)
      | None -> acc
  in
  List.fold_left (fun acc (num, texts) ->
    let indices = List.fold_left (fun acc text ->
      find_from text 0 acc
    ) [] texts in
    (num, List.rev indices) :: acc
  ) [] patterns

let flatten_and_sort_indices indices =
  List.fold_left (fun acc (num, idxs) ->
    List.fold_left (fun acc idx -> (num, idx) :: acc) acc idxs
  ) [] indices
  |> List.sort (fun (_, idx1) (_, idx2) -> compare idx1 idx2)

  let create_ordered_string indices =
    let flattened_indices = flatten_and_sort_indices indices in
    let numbers_in_order = List.map (fun (num, _) -> string_of_int num) flattened_indices in
    String.concat "" numbers_in_order

let split_string_at_zero str =
  Str.split (Str.regexp "0") str

let string_ends str =
  String.make 1 str.[0] ^ String.make 1 str.[String.length str - 1]

let convert_to_int_list str_list =
  List.map int_of_string str_list
      

let main () =
  let text = d01_1 in
  let patterns = [
    (1, ["one"; "1"]);
    (2, ["two"; "2"]);
    (3, ["three"; "3"]);
    (4, ["four"; "4"]);
    (5, ["five"; "5"]);
    (6, ["six"; "6"]);
    (7, ["seven"; "7"]);
    (8, ["eight"; "8"]);
    (9, ["nine"; "9"]);
    (0, ["\n"]);
  ] in
  let indices = find_all_indices text patterns in
  let ordered_string = create_ordered_string indices in
  let split_list = split_string_at_zero ordered_string in
  let ends_list = List.map string_ends split_list in
  let int_list = convert_to_int_list ends_list in
  let () = List.iter print_endline ends_list in
  let sum_num = List.fold_left ( + ) 0 int_list in
  let () = print_int sum_num in
  ();
