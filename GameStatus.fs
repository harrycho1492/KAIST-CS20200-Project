namespace TermProject

type GameStatus () =
  // tile status index
  //       0 : hidden
  //  1 ~ 10 : bonus tile 1 ~ 10 (1 ~ 5: shown / 6 ~ 10: hidden)
  // 11 ~ 13 : in hand of player 1 ~ 3
  // 14 ~ 16 : open in front of player 1 ~ 3
  // 17 ~ 19 : discarded in front of player 1 ~ 3
  //      20 : unused
  let tiles = Array.create 108 0
  // for furiten
  // Player 1 :  0 ~ 26
  // Player 2 : 27 ~ 53
  // Player 3 : 54 ~ 80
  let hasDiscarded = Array.create 81 false
  // TBA - temporary furiten memory
  // for bonus tile display &  four quads abort
  let numQuads = 0

  // for tile draws
  let rand = System.Random ()
  let rec drawTile () =
    let idx = rand.Next (0, 108)
    if (not (tiles[idx] = 0)) then (drawTile ()) else (idx)
  
  // internally used functions
  let assertVaildPlayer n =
    if ((n > 3) || (n < 1)) then (failwith "Fatal error") else ()
  let tileNotation idx =
    match idx / 4 with
    | 0  -> "1m"
    | 1  -> "9m"
    | 2  -> "1p"
    | 3  -> "2p"
    | 4  -> "3p"
    | 5  -> "4p"
    | 6  -> if (idx % 4 = 0) then ("5P") else ("5p") // Red tile bonus
    | 7  -> "6p"
    | 8  -> "7p"
    | 9  -> "8p"
    | 10 -> "9p"
    | 11 -> "1s"
    | 12 -> "2s"
    | 13 -> "3s"
    | 14 -> "4s"
    | 15 -> if (idx % 4 = 0) then ("5S") else ("5s") // Red tile bonus
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
  let remainingTiles () =
    Array.fold (fun x y -> if (y = 0) then (x + 1) else (x)) 0 tiles
  let getHandTiles n =
    assertVaildPlayer n
    Array.map (fun (x, _) -> x) (Array.filter (fun (x, y) -> y = (10 + n)) (Array.indexed tiles))
  let displayBonusTiles n =
    printfn "Bonus tiles:"
    match n with
    | -1 ->
      let b1 = [ 1..5 ]  |> List.map (fun x -> tileNotation (Array.findIndex (fun y -> y = x) tiles))
      let b2 = [ 6..10 ] |> List.map (fun x -> tileNotation (Array.findIndex (fun y -> y = x) tiles))
      printfn "%s" (List.fold (fun x y -> x + " | " + y) b1.Head b1.Tail)
      printfn "%s\n" (List.fold (fun x y -> x + " | " + y) b2.Head b2.Tail)
    | 1 ->
      printfn "%s\n" (tileNotation (Array.findIndex (fun y -> y = 1) tiles))
    | 2 | 3 | 4 | 5 ->
      let b1 = [ 1..n ]  |> List.map (fun x -> tileNotation (Array.findIndex (fun y -> y = x) tiles))
      printfn "%s\n" (List.fold (fun x y -> x + " | " + y) b1.Head b1.Tail)
    | _  -> failwith "Fatal error"
  let displayHandTiles n =
    let ht = Array.toList (Array.map (fun x -> tileNotation x) (getHandTiles n))
    match List.length ht with
    | 1            -> printfn "%s\n" ht.Head
    | x when x > 1 -> printfn "%s\n" (List.fold (fun x y -> x + " | " + y) ht.Head ht.Tail)
    | _            -> failwith "Fatal error"
  let displayDiscardedTiles n =
    assertVaildPlayer n
    let dt = Array.toList (
      Array.map (fun (x, _) -> tileNotation x) (Array.filter (fun (x, y) -> y = (16 + n)) (Array.indexed tiles))
    )
    match List.length dt with
    | 0            -> printfn "None"
    | 1            -> printfn "%s\n" dt.Head
    | x when x > 1 -> printfn "%s\n" (List.fold (fun x y -> x + " | " + y) dt.Head dt.Tail)
    | _            -> failwith "Fatal error"

  member __.Init () =
    // Getting bonus tiles
    for i in 1..10 do
      tiles[drawTile ()] <- i
    // Getting unused tiles
    for i in 1..5 do
      tiles[drawTile ()] <- 20
    // Getting initial hand tiles
    for i in 1..39 do
      tiles[drawTile ()] <- 11 + ((i - 1) % 3)
    ()
  
  member __.CanDrawTile () =
    Array.exists (fun x -> x = 0) tiles

  member __.DrawTile player =
    assertVaildPlayer player
    let drawnTile = drawTile ()
    tiles[drawnTile] <- 10 + player
    tileNotation drawnTile
  
  member __.DiscardTile idx player =
    assertVaildPlayer player
    if (not (tiles[idx] = 10 + player)) then (failwith "Invalid discard")
    tiles[idx] <- 16 + player
    tileNotation idx

  member __.ConstructInfo viewPoint =
    getHandTiles viewPoint

  member __.Display viewPoint =
    match viewPoint with
    | 0 -> // All-seeing observer perspective
      displayBonusTiles -1
      for i in 1..3 do
        printfn "In player %d's hand:" i
        displayHandTiles i
        printfn "Discarded by player %d:" i
        displayDiscardedTiles i
      // TBA - How to display open tiles
      printfn "%d tiles remaining in the stack\n" (remainingTiles ())
    | 1 | 2 | 3 ->
      displayBonusTiles (1 + numQuads)
      printfn "In your hand:"
      displayHandTiles viewPoint
      printfn "Discarded by you:"
      displayDiscardedTiles viewPoint
      for i in 1..3 do
        if (not (i = viewPoint)) then (
          printfn "Discarded by player %d:" i
          displayDiscardedTiles i
        )
      printfn "%d tiles remaining in the stack\n" (remainingTiles ())
    | _ -> failwith "Fatal error"

  // TBA - More methods to change and/or show game status