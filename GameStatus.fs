namespace TermProject

type GameStatus () =
  // tile status index
  //       0 : hidden
  //  1 ~ 10 : bonus tile 1 ~ 10 (1 ~ 5: shown / 6 ~ 10: hidden)
  // 11 ~ 13 : in hand of player 1 ~ 3
  // 14 ~ 16 : open in front of player 1 ~ 3
  // 17 ~ 19 : discarded in front of player 1 ~ 3
  let tiles = Array.create 108 0
  // for furiten
  // Player 1 :  0 ~ 26
  // Player 2 : 27 ~ 53
  // Player 3 : 54 ~ 80
  let hasDiscarded = Array.create 81 false
  // for four quads abort
  let numQuads = 0

  member __.Init () =
    let rand = System.Random ()
    let rec getIdx () =
      let idx = rand.Next (0, 108)
      if (not (tiles[idx] = 0)) then (getIdx ()) else (idx)
    // Getting bonus tiles
    for i in 1..10 do
      tiles[getIdx ()] <- i
    // Getting initial hand tiles
    for i in 1..39 do
      tiles[getIdx ()] <- 11 + ((i - 1) % 3)
    ()

  member __.ConstructInfo viewPoint =
    // For bot algorithm building, TBA
    ()

  member __.Display viewPoint =
    let tileNotation n =
      match n / 4 with
      | 0  -> "1m"
      | 1  -> "9m"
      | 2  -> "1p"
      | 3  -> "2p"
      | 4  -> "3p"
      | 5  -> "4p"
      | 6  -> if (n % 4 = 0) then ("5P") else ("5p") // Red tile bonus
      | 7  -> "6p"
      | 8  -> "7p"
      | 9  -> "8p"
      | 10 -> "9p"
      | 11 -> "1s"
      | 12 -> "2s"
      | 13 -> "3s"
      | 14 -> "4s"
      | 15 -> if (n % 4 = 0) then ("5S") else ("5s") // Red tile bonus
      | 16 -> "6s"
      | 17 -> "7s"
      | 18 -> "8s"
      | 19 -> "9s"
      | 20 -> "1z"
      | 21 -> "2z"
      | 22 -> "3z"
      | 23 -> "4z"
      | 24 -> "5z"
      | 25 -> "6z"
      | 26 -> "7z"
      | _  -> failwith "Fatal error"
    match viewPoint with
    | 0 -> // All-seeing observer perspective
      printfn "Bonus tiles"
      let bonus = [| 1..10 |] |> Array.map (fun x -> tileNotation (Array.findIndex (fun y -> y = x) tiles))
      printfn "%s | %s | %s | %s | %s" bonus[0] bonus[1] bonus[2] bonus[3] bonus[4]
      printfn "%s | %s | %s | %s | %s" bonus[5] bonus[6] bonus[7] bonus[8] bonus[9]
      printfn ""
      for i in 1..3 do
        printfn "In player %d's hand:" i
        let ht = Array.map (fun (x, _) -> tileNotation x) (Array.filter (fun (x, y) -> y = (10 + i)) (Array.indexed tiles))
        printfn "%s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s\n" ht[0] ht[1] ht[2] ht[3] ht[4] ht[5] ht[6] ht[7] ht[8] ht[9] ht[10] ht[11] ht[12]
      // TBA - How to display open tiles & discarded tiles
    | 1 -> failwith "Not implemented"
    | 2 -> failwith "Not implemented"
    | 3 -> failwith "Not implemented"
    | _ -> failwith "Fatal error"

  // TBA - More methods to change and/or show game status