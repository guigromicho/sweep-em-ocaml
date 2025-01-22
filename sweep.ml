(*celulas do tablueiro*)
type cell =
  | Bomb
  | Number of int

(*Estado de cada celula do tabuleiro*)
type state =
  | Hidden
  | Revealed
  | Marked
(*cria cada celula do tabuleiro*)
type board = {
  cells: (cell * state) array array;
  size: int;
}
(*função que executa a jogada*)
let rec play board x y =

  match board.cells.(x).(y) with
  | (Bomb, Hidden) -> 
      print_endline "Pisaste uma mina!"; 
      exit 0
  | (Number n, Hidden) ->
      if n = 0 then (
        board.cells.(x).(y) <- (Number n, Revealed);
        let directions = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)] in
        List.iter
          (fun (dx, dy) ->
            let nx, ny = (x + dx, y + dy) in
            if nx >= 0 && ny >= 0 && nx < board.size && ny < board.size then
              match board.cells.(nx).(ny) with
              | (_, Hidden) -> play board nx ny
              | _ -> ()
          )
          directions
      )
      else
        board.cells.(x).(y) <- (Number n, Revealed)
  | (_, Revealed) | (_, Marked) -> ()

(*Função que faz print do tabuleiro*)
let print_board board =
  Printf.printf "   ";
  Array.iteri (fun col_index _ -> Printf.printf " \027[35m%d\027[0m " (col_index + 1)) board.cells.(0);
  print_endline "";
  Array.iteri (fun row_index row ->
    Printf.printf "\027[35m%d\027[0m " (row_index + 1);
    print_string "(";
    Array.iter (fun (cell, state) ->
      match (cell, state) with
      | (_, Hidden) -> print_string " \027[31mX\027[0m "
      | (Bomb, Revealed) -> print_string " # "
      | (Number 0, Revealed) -> print_string "   "
      | (Number n, Revealed) -> Printf.printf " \027[34m%d\027[0m " n 
      | (_, Marked) -> print_string " \027[92mM\027[0m "
    ) row;
    print_endline ")"
  ) board.cells

(*função que calcula o número de minas à volta da celula*)
let increment_neighbors board x y =
  let directions = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)] in
  List.iter
    (fun (dx, dy) ->
      let nx, ny = (x + dx, y + dy) in
      if nx >= 0 && ny >= 0 && nx < board.size && ny < board.size then
        match board.cells.(nx).(ny) with
        | (Number n, state) -> board.cells.(nx).(ny) <- (Number (n + 1), state)
        | _ -> ())
    directions

(*Limpa o número de minas de cada célula*)
let reset_numbers board =
  Array.iteri (fun x row ->
    Array.iteri (fun y (cell, state) ->
      match cell with
      | Number _ -> board.cells.(x).(y) <- (Number 0, state)
      | _ -> ()
    ) row
  ) board.cells

(*Vê se todas as minas já foram marcadas*)
let is_done board = 
  let count = ref 0 in
  Array.iter (fun row ->
    Array.iter (fun (cell, state) ->
      match (cell, state) with
      | (Bomb, Hidden) -> count := !count + 1
      | (Number n, Marked) -> count := !count + 1
      | _ -> ()
    ) row
  ) board.cells;
  !count = 0
let cheat board =
  Printf.printf "   ";
  Array.iteri (fun col_index _ -> Printf.printf " \027[35m%d\027[0m " (col_index + 1)) board.cells.(0);
  print_endline "";
  Array.iteri (fun row_index row ->
    Printf.printf "\027[35m%d \027[0m" (row_index + 1);
    print_string "(";
    Array.iter (fun (cell, state) ->
      match (cell, state) with
      | (Bomb, Marked) -> print_string " \027[32m#\027[0m "
      | (Bomb, _) -> print_string " \027[31m#\027[0m "
      | (Number n, Marked) -> print_string " \027[33mM\027[0m "
      | (Number 0, _) -> print_string "   "
      | (Number n, _) -> Printf.printf " %d " n
    ) row;
    print_endline ")"
  ) board.cells

(*Função que corre cada casa do tabuleiro e chama a função increment_neighbors*)
let calculate_numbers board =
  reset_numbers board;
  Array.iteri
    (fun x row ->
      Array.iteri
        (fun y (cell, _) ->
          match cell with
          | Bomb -> increment_neighbors board x y
          | _ -> ())
        row)
    board.cells


(*Cria o tabuleiro*)
let create_board n =
  if n <= 0 then failwith "Tamanho do tabuleiro tem que ser maior que 0";
  { cells = Array.init n (fun _ -> Array.init n (fun _ -> (Number 0, Hidden))); size = n }

(*Coloca k minas no tabuleiro*)
let place_bombs board k =
  let bomb_count = ref 0 in
  let n = board.size in
  Random.self_init ();

  while !bomb_count < k do
    let x = Random.int n in
    let y = Random.int n in

    match fst board.cells.(x).(y) with
    | Bomb -> ()
    | Number _ ->
        board.cells.(x).(y) <- (Bomb, Hidden);
        bomb_count := !bomb_count + 1
  done;
  print_endline (string_of_int k ^ " minas plantada!")

(*função principal do codigo*)
let () =
  let board_ref = ref None in

  while true do
    print_endline "\027[36mEscreve um comando:\027[37m";
    let command = read_line () in

    let tokenize line = 
      String.split_on_char ' ' line |> List.filter (fun s -> s <> "") 
    in

    let tokens = tokenize command in

    match tokens with
    | "empty" :: n_str :: [] ->
        begin
          try
            let n = int_of_string n_str in
            board_ref := Some (create_board n);
            print_endline ("Tabuleiro de tamanho " ^ string_of_int n ^ " criado!");
          with Failure _ -> 
            print_endline "Valor errado para tamanho de tabuleiro. Escreva um número inteiro"
        end
    
    | "random" :: n_str :: [] ->
        begin
          match !board_ref with
          | None -> print_endline "Não existe um tabuleiro."
          | Some board -> 
              try
                let k = int_of_string n_str in 
                if k > board.size * board.size then
                  print_endline "Quantidade de minas excede o tamanho do tabuleiro."
                else begin
                  place_bombs board k;
                  calculate_numbers board;
                  print_endline ("Minas plantadas no tabuleiro");
                end
              with Failure _ -> 
                print_endline "Valor errado para número de minas. Escreve um número inteiro."
        end

    | "step" :: row_str :: col_str :: [] ->
        begin
          match !board_ref with
          | None -> print_endline "Não existe um tabuleiro."
          | Some board -> 
              try
                let x = int_of_string row_str in
                let y = int_of_string col_str in
                let x = x - 1 in
                let y = y - 1 in
                if x < 0 || x >= (board.size + 1) || y < 0 || y >= (board.size + 1) then
                  print_endline "Cordenadas fora do tabuleiro"
                else
                  match fst board.cells.(x).(y) with
                  | Bomb -> print_endline "Existe uma mina nestas cordenadas"
                  | Number n -> print_endline ("Existem " ^ string_of_int n ^ " minas por perto.")
              with Failure _ -> 
                print_endline "Entrada inválida. Por favor, insere números inteiros válidos para a linha e para a coluna"
        end

    | "mark" :: row_str :: col_str :: [] ->
      begin
        match !board_ref with
        | None -> print_endline "Não existe um tabuleiro."
        | Some board ->
          try
            let x = int_of_string row_str in
            let y = int_of_string col_str in
            let x = x - 1 in
            let y = y - 1 in 
            if x < 0 || x >= (board.size + 1) || y < 0 || y >= (board.size + 1) then
              print_endline "Cordenadas fora do tabuleiro"
            else
              match board.cells.(x).(y) with
              | (_, Marked) -> print_endline "Célula já foi marcada"
              | (_, Revealed) -> print_endline "Célula já foi revelada portanto não pode ser marcada."	
              | _ ->
                  board.cells.(x).(y) <- (fst board.cells.(x).(y), Marked);
                  print_endline "Célula marcada com sucesso"
          with Failure _ -> print_endline "Entrada inválida. Por favor, insere números inteiros válidos para a linha e para a coluna"
      end
    | "unmark" :: row_str :: col_str :: [] ->
        begin
          match !board_ref with
          | None -> print_endline "Não existe um tabuleiro."
          | Some board ->
              try
                let x = int_of_string row_str in
                let y = int_of_string col_str in
                let x = x - 1 in  
                let y = y - 1 in 
                if x < 0 || x >= (board.size + 1) || y < 0 || y >= (board.size + 1) then
                  print_endline "Cordenadas fora do tabuleiro"
                else
                  match board.cells.(x).(y) with
                  | (_, Marked) -> board.cells.(x).(y) <- (fst board.cells.(x).(y), Hidden); print_endline "Célula desmarcada com sucesso"
                  | (_, Revealed) -> print_endline "Célula já foi revelada portanto não pode ser desmarcada."
                  | _ -> print_endline "Célula não está marcada."
              with Failure _ -> print_endline "Entrada inválida. Por favor, insere números inteiros válidos para a linha e para a coluna"
        end
    | "done" :: [] ->
      begin
        match !board_ref with
        | None -> print_endline "Não existe um tabuleiro."
        | Some board ->
            if is_done board then
              begin
                print_endline "Ganhaste!";
                exit 0
              end
            else
              print_endline "Ainda não marcaste todas as minas, ou marcaste casas sem minas."
      end

    | "play" :: row_str :: column_str :: [] ->
        begin
          try
            match !board_ref with
            | None -> print_endline "Não existe um tabuleiro."
            | Some board ->
                let x = int_of_string row_str in
                let y = int_of_string column_str in
                if x < 1 || x >= (board.size + 1) || y < 1 || y >= (board.size + 1) then
                  print_endline "Cordenadas fora do tabuleiro"
                else
                  play board (x - 1) (y - 1)
          with Failure _ -> print_endline "Entrada inválida. Por favor, insere números inteiros válidos para a linha e para a coluna"
        end
    | "dump" :: [] -> 
        begin
          match !board_ref with
          | None -> print_endline "Não existe um tabuleiro."
          | Some board -> 
            print_board board;
        end
    | "mine" :: row_str :: col_str :: [] ->
        begin
          match !board_ref with
          | None -> print_endline "Não existe um tabuleiro."
          | Some board ->
              try
                let x = int_of_string row_str in
                let y = int_of_string col_str in
                let x = x - 1 in
                let y = y - 1 in 
                if x < 0 || x >= (board.size + 1) || y < 0 || y >= (board.size + 1) then
                  print_endline "Cordenadas fora do tabuleiro"
                else begin
                  match fst board.cells.(x).(y) with
                  | Bomb ->
                      print_endline "Já existe uma mina aqui."
                  | Number _ ->
                      board.cells.(x).(y) <- (Bomb, Hidden);
                      calculate_numbers board;
                      print_endline ("Mina colocada em (" ^ (string_of_int (x + 1))^ ", " ^ (string_of_int (y + 1))^ ").")
                end
              with Failure _ -> 
                print_endline "Entrada inválida. Por favor, insere números inteiros válidos para a linha e para a coluna"
        end
    | "exit" :: [] -> 
        print_endline "A sair do jogo."; 
        exit 0
    | "help" :: [] ->
      print_endline "empty N -> Cria um novo tabuleiro";
      print_endline "random N -> Coloca minas no tabuleiro";
      print_endline "mine R C -> Coloca uma mina na linha R, coluna C";
      print_endline "step R C-> \"Testa\" o valor da cordenada R C";
      print_endline "mark R C -> Marca a linha R, coluna C com suspeita de mina";
      print_endline "unmark R C -> Remove a marcação da linha R, coluna C";
      print_endline "done -> Verifica se o jogo está completo";
      print_endline "play R C -> faz a jogada na linha R, coluna C";
      print_endline "dump -> Imprime o tabuleiro no terminal";
      print_endline "cheat -> Imprime o tabuleiro com todas as posições reveladas";
      print_endline "exit -> Sai do jogo";
    | "cheat" :: [] ->
      begin
        match !board_ref with
        | None -> print_endline "Não existe um tabuleiro."
        | Some board -> 
            cheat board
      end
    | _ -> 
        print_endline "Este comando não existe";
  done