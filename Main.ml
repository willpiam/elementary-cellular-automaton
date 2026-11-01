let die msg =
  prerr_endline msg;
  exit 1

let parse_int text =
  let t = String.trim text in
  let neg, digits =
    if t <> "" && t.[0] = '-' then true, String.sub t 1 (String.length t - 1)
    else false, t
  in
  if digits = "" || not (String.for_all (fun c -> c >= '0' && c <= '9') digits) then None
  else
    try
      let value = int_of_string digits in
      Some (if neg then -value else value)
    with Failure _ -> None

let valid_seed seed =
  seed <> "" && String.for_all (fun c -> c = '0' || c = '1') seed

let read_inputs path =
  let ic = try open_in path with Sys_error _ -> die "cannot read input.txt" in
  let line () = try Some (input_line ic) with End_of_file -> None in
  let rule_line = line () in
  let seed = line () in
  let gen_line = line () in
  close_in ic;
  match rule_line, seed, gen_line with
  | Some a, Some b, Some c -> (a, b, c)
  | _ -> die "invalid input"

let () =
  let rule_line, seed, gen_line = read_inputs "input.txt" in
  let rule = match parse_int rule_line with Some v when v >= 0 && v <= 255 -> v | _ -> die "invalid rule" in
  if not (valid_seed seed) then die "invalid initial conditions";
  let generations = match parse_int gen_line with Some v when v >= 1 -> v | _ -> die "invalid generations" in
  let n = String.length seed in
  let width =
    try
      let w = n + 2 * generations in
      if w < n then die "invalid generations" else w
    with Invalid_argument _ -> die "invalid generations"
  in
  let table = Array.init 8 (fun i -> (rule lsr i) land 1) in
  let even = rule land 1 = 0 in
  let prev = Array.make width 0 in
  let next_row = Array.make width 0 in
  for i = 0 to n - 1 do
    if seed.[i] = '1' then prev.(generations + i) <- 1
  done;
  let (_ : int) = Sys.command "mkdir -p results" in
  let path = Printf.sprintf "results/r%d_g%d_i%s_ocaml.pbm" rule generations seed in
  let oc = try open_out_bin path with Sys_error _ -> die "cannot write output" in
  Printf.fprintf oc "P1\n%d %d\n" width generations;
  for gen = 0 to generations - 1 do
    for j = 0 to width - 1 do
      output_char oc (if prev.(j) = 0 then '0' else '1')
    done;
    output_char oc '\n';
    if gen + 1 = generations then ()
    else begin
      Array.fill next_row 0 width 0;
      let lo, hi =
        if even then (generations - (gen + 1), generations + n + (gen + 1))
        else (0, width)
      in
      for j = lo to hi - 1 do
        let left = if j > 0 then prev.(j - 1) else 0 in
        let right = if j + 1 < width then prev.(j + 1) else 0 in
        let idx = (left lsl 2) lor (prev.(j) lsl 1) lor right in
        next_row.(j) <- table.(idx)
      done;
      Array.blit next_row 0 prev 0 width
    end
  done;
  close_out oc
