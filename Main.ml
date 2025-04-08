(* Convert rule number to binary array *)
let rule_to_binary_array rule_number =
  let binary = Bytes.make 8 '0' in
  for i = 0 to 7 do
    if (rule_number lsr i) land 1 = 1 then
      Bytes.set binary (7-i) '1'
  done;
  Array.init 8 (fun i -> int_of_char (Bytes.get binary i) - int_of_char '0')

(* Calculate next cell state *)
let calculate_cell neighborhood rule =
  let pattern_to_index = function
    | "111" -> 0
    | "110" -> 1
    | "101" -> 2
    | "100" -> 3
    | "011" -> 4
    | "010" -> 5
    | "001" -> 6
    | "000" -> 7
    | _ -> failwith "Invalid neighborhood pattern"
  in
  rule.(pattern_to_index neighborhood)

(* Run the cellular automaton *)
let run_cellular_automaton rule generations initial_cells =
  let cells = Array.copy initial_cells in
  let image_width = Array.length cells + 2 * generations in
  let ca = Array.make_matrix generations image_width 0 in
  
  (* Initialize first generation *)
  let padding_length = (image_width - Array.length cells) / 2 in
  Array.blit cells 0 ca.(0) padding_length (Array.length cells);
  
  (* Generate subsequent generations *)
  for i = 1 to generations - 1 do
    for j = 1 to image_width - 2 do
      let neighborhood = Printf.sprintf "%d%d%d" 
        ca.(i-1).(j-1) ca.(i-1).(j) ca.(i-1).(j+1) in
      ca.(i).(j) <- calculate_cell neighborhood rule
    done
  done;
  ca

(* Read input from file *)
let read_inputs_from_file file_path =
  let ic = open_in file_path in
  let rule_number = int_of_string (input_line ic) in
  let initial_conditions = input_line ic in
  let generations = int_of_string (input_line ic) in
  close_in ic;
  (rule_number, initial_conditions, generations)

(* Main program *)
let () =
  try
    (* Read input *)
    let rule_number, initial_conditions, generations = read_inputs_from_file "input.txt" in

    (* Convert initial conditions to array *)
    let initial_cells = Array.init (String.length initial_conditions)
      (fun i -> int_of_char initial_conditions.[i] - int_of_char '0') in

    (* Run automaton *)
    let rule = rule_to_binary_array rule_number in
    let ca = run_cellular_automaton rule generations initial_cells in

    (* Write output *)
    let oc = open_out (Printf.sprintf "results/r%d_g%d_i%s_ocaml.pbm" 
      rule_number generations initial_conditions) in
    Printf.fprintf oc "P1\n%d %d\n" 
      (Array.length ca.(0)) generations;
    Array.iter (fun row ->
      Array.iter (Printf.fprintf oc "%d") row;
      Printf.fprintf oc "\n"
    ) ca;
    close_out oc
  with
  | Sys_error msg -> Printf.eprintf "Error: %s\n" msg
  | Failure msg -> Printf.eprintf "Error: %s\n" msg
  | _ -> Printf.eprintf "An unexpected error occurred\n" 