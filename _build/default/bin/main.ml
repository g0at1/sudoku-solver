type cell = int
type board = cell array array

let empty_board () : board =
  Array.init 9 (fun _ -> Array.make 9 0)

let row_values (b : board) r = Array.to_list b.(r)
let col_values (b : board) c = List.init 9 (fun r -> b.(r).(c))
let block_values (b : board) r c =
  let br = (r / 3) * 3 and bc = (c / 3) * 3 in
  List.concat (
    List.init 3 (fun dr ->
      List.init 3 (fun dc ->
        b.(br + dr).(bc + dc)
      )
    )
  )

let can_place (b : board) r c v =
  v <> 0 &&
  not (List.mem v (row_values b r)) &&
  not (List.mem v (col_values b c)) &&
  not (List.mem v (block_values b r c))

let find_empty (b : board) =
  let rec aux r c =
    if r = 9 then None
    else if c = 9 then aux (r+1) 0
    else if b.(r).(c) = 0 then Some (r,c)
    else aux r (c+1)
  in
  aux 0 0

let rec solve (b : board) : bool =
  match find_empty b with
  | None -> true
  | Some (r, c) ->
      let rec try_vals v =
        if v > 9 then false
        else if can_place b r c v then begin
          b.(r).(c) <- v;
          if solve b then true
          else (b.(r).(c) <- 0; try_vals (v+1))
        end else
          try_vals (v+1)
      in
      try_vals 1

let read_board () : board =
  let b = empty_board () in
  for r = 0 to 8 do
    Scanf.scanf " %d %d %d %d %d %d %d %d %d\n"
      (fun c0 c1 c2 c3 c4 c5 c6 c7 c8 ->
         b.(r).(0)<-c0; b.(r).(1)<-c1; b.(r).(2)<-c2;
         b.(r).(3)<-c3; b.(r).(4)<-c4; b.(r).(5)<-c5;
         b.(r).(6)<-c6; b.(r).(7)<-c7; b.(r).(8)<-c8)
  done;
  b

let print_board (b : board) =
  for r = 0 to 8 do
    for c = 0 to 8 do
      let v = b.(r).(c) in
      print_int v;
      if c = 2 || c = 5 then print_string " | "
      else if c < 8 then print_string " "
    done;
    print_newline ();
    if r = 2 || r = 5 then print_endline "------+-------+------"
  done

let () =
  print_endline "Enter your board (0 = empty):";
  let board = read_board () in
  print_endline "\nInput board:";
  print_board board;
  if solve board then begin
    print_endline "\nOutput board:";
    print_board board
  end else
    print_endline "No solution for provided board."
