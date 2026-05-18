namespace TermProject

type GameStatus () =
  // tile status index
  //        -1 : unused
  //         0 : hidden
  //   1 ~  10 : bonus tile 1 ~ 10 (1 ~ 5: shown / 6 ~ 10: hidden)
  //  11 ~  13 : in hand of player 1 ~ 3
  //  14 ~  16 : just drawn by player 1 ~ 3
  //  17 ~  19 : north bonus tiles in front of player 1 ~ 3
  //  21 ~  23 : closed quad    by player 1 ~ 3
  //  32 &  33 : open   quad    by player 2 & 3, loaned from player 1
  //  34 &  36 : open   quad    by player 1 & 3, loaned from player 2
  //  37 &  28 : open   quad    by player 1 & 2, loaned from player 3
  //  42 &  43 : added  quad    by player 2 & 3, loaned from player 1
  //  44 &  46 : added  quad    by player 1 & 3, loaned from player 2
  //  47 &  48 : added  quad    by player 1 & 2, loaned from player 3
  //  52 &  53 : called triplet by player 2 & 3, loaned from player 1
  //  54 &  56 : called triplet by player 1 & 3, loaned from player 2
  //  57 &  58 : called triplet by player 1 & 2, loaned from player 3
  // 101 ~ 154 : discarded in front of player 1, numbered in order
  // 201 ~ 254 : discarded in front of player 1, numbered in order
  // 301 ~ 354 : discarded in front of player 1, numbered in order
  let tiles = Array.create 108 0
  // for bonus tile display &  four quads abort
  let mutable numQuads = 0
  // for nine different terminal and honor tiles abort
  // and "Blessing of Heaven" & "Blessing of Earth" limit hands
  let mutable firstRound = 1

  // for tile draws
  let rand = System.Random ()
  let rec drawTile fromUnused =
    if (fromUnused) then (
      Array.findIndex (fun x -> x = -1) tiles
    ) else (
      let candidates =
        Array.map (fun (x, _) -> x) (Array.filter (fun (_, y) -> y = 0) (Array.indexed tiles))
      candidates[rand.Next (0, Array.length candidates)]
    )
  
  // internally used functions
  let assertVaildPlayer n =
    if ((n > 3) || (n < 1)) then (failwith "Fatal error") else ()
  let tileNotation idx =
    match idx / 4 with
    |  0 -> "1m"
    |  1 -> "9m"
    |  2 -> "1p"
    |  3 -> "2p"
    |  4 -> "3p"
    |  5 -> "4p"
    |  6 -> if (idx % 4 = 0) then ("5P") else ("5p") // Red tile bonus
    |  7 -> "6p"
    |  8 -> "7p"
    |  9 -> "8p"
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
  let discardedTiles n =
    assertVaildPlayer n
    Array.fold (fun x y -> if ((y > n * 100) && (y < n * 100 + 55)) then (x + 1) else (x)) 0 tiles
  let getHandTiles n =
    assertVaildPlayer n
    Array.map (fun (x, _) -> x) (Array.filter (fun (_, y) -> y = (10 + n)) (Array.indexed tiles))
  let getClosedQuads n =
    if (Array.contains (20 + n) tiles) then (
      let temp = Array.map (fun (x, _) -> x) (Array.filter (fun (_, y) -> y = (20 + n)) (Array.indexed tiles))
      if (not ((Array.length temp) % 4 = 0)) then (
        failwith "Fatal error"
      ) else (
        Array.map (fun x -> (x / 4, 20 + n)) (Array.filter (fun x -> x % 4 = 0) temp)
      )
    ) else ( [| |] )
  let getOpenOrAddedQuads isLatter n =
    let (na, nb) =
      match n with
      | 1 -> if (isLatter) then (44, 47) else (34, 37)
      | 2 -> if (isLatter) then (42, 48) else (32, 38)
      | 3 -> if (isLatter) then (43, 46) else (33, 36)
      | _ -> failwith "Fatal error"
    if ((Array.contains na tiles) || (Array.contains nb tiles)) then (
      let ta = Array.map (fun (x, _) -> x) (Array.filter (fun (_, y) -> y = na) (Array.indexed tiles))
      let tb = Array.map (fun (x, _) -> x) (Array.filter (fun (_, y) -> y = nb) (Array.indexed tiles))
      if ((not ((Array.length ta) % 4 = 0)) || (not ((Array.length ta) % 4 = 0))) then (
        failwith "Fatal error"
      ) else (
        Array.append
          (Array.map (fun x -> (x / 4, na)) (Array.filter (fun x -> x % 4 = 0) ta))
          (Array.map (fun x -> (x / 4, nb)) (Array.filter (fun x -> x % 4 = 0) tb))
      )
    ) else ( [| |] )
  let getCalledTriplets n =
    let (na, nb) =
      match n with
      | 1 -> (54, 57)
      | 2 -> (52, 58)
      | 3 -> (53, 56)
      | _ -> failwith "Fatal error"
    let ta = Array.map (fun (x, _) -> x) (Array.filter (fun (_, y) -> y = na) (Array.indexed tiles))
    let tb = Array.map (fun (x, _) -> x) (Array.filter (fun (_, y) -> y = nb) (Array.indexed tiles))
    if ((not ((Array.length ta) % 3 = 0)) || (not ((Array.length ta) % 3 = 0))) then (
      failwith "Fatal error"
    ) else (
      Array.append
        (Array.map (fun (_, y) -> (y / 4, na)) (Array.filter (fun (x, _) -> x % 3 = 2) (Array.indexed ta)))
        (Array.map (fun (_, y) -> (y / 4, nb)) (Array.filter (fun (x, _) -> x % 3 = 2) (Array.indexed tb)))
    )
  let getMeldTiles n =
    Array.toList (
      Array.append
        (Array.append (getClosedQuads n)   (getOpenOrAddedQuads false n))
        (Array.append (getOpenOrAddedQuads true n) (getCalledTriplets n))
    )
  let getDrawnTile n =
    if (Array.contains (13 + n) tiles) then (Array.findIndex (fun x -> x = 13 + n) tiles) else (-1)
  let displayBonusTiles n =
    printfn "Bonus tiles:"
    match n with
    | 0 ->
      let b1 = [ 1..5 ]  |> List.map (fun x -> tileNotation (Array.findIndex (fun y -> y = x) tiles))
      let b2 = [ 6..10 ] |> List.map (fun x -> tileNotation (Array.findIndex (fun y -> y = x) tiles))
      printfn "%s" (List.fold (fun x y -> x + " | " + y) b1.Head b1.Tail)
      printfn "%s\n" (List.fold (fun x y -> x + " | " + y) b2.Head b2.Tail)
    | 1 | 2 | 3 | 4 | 5 ->
      let b1 = [ 1..5 ]  |> List.map (fun x ->
        if (x > n) then ("??") else (tileNotation (Array.findIndex (fun y -> y = x) tiles)))
      printfn "%s\n" (List.fold (fun x y -> x + " | " + y) b1.Head b1.Tail)
    | _  -> failwith "Fatal error"
  let displayHandTiles n index =
    assertVaildPlayer n
    let ht = Array.toList (Array.map (fun x -> tileNotation x) (getHandTiles n))
    let dt = getDrawnTile n
    match (List.length ht, dt) with
    | (1, -1) ->
      printfn "%s" ht.Head
      if (index) then (printfn " 1\n") else (printfn "")
    | (1,  y) ->
      printfn "%s | %s" ht.Head (tileNotation y)
      if (index) then (printfn " 1   14\n") else (printfn "")
    | (x, -1) when x > 1 ->
      printfn "%s" (List.fold (fun x y -> x + " | " + y) ht.Head ht.Tail)
      if (index) then (
        for i in 1..x do printf "%2d   " i
        printfn "\n"
      ) else (printfn "")
    | (x,  y) ->
      printfn "%s | %s" (List.fold (fun x y -> x + " | " + y) ht.Head ht.Tail) (tileNotation y)
      if (index) then (
        for i in 1..x do printf "%2d   " i
        printfn "14\n"
      ) else (printfn "")
  let displayMeldTiles n =
    assertVaildPlayer n
    let mt = getMeldTiles n
    match List.length mt with
    | 0 -> printfn "None\n"
    | _ ->
      printfn ""
      (List.map (
        fun (x, y) ->
          match y with
          | 21 | 22 | 23 -> printfn "Closed quad of %s" (tileNotation (4 * x + 3))
          | 32 | 33      -> printfn "Open quad of %s, loaned from player 1" (tileNotation (4 * x + 3))
          | 34 | 36      -> printfn "Open quad of %s, loaned from player 2" (tileNotation (4 * x + 3))
          | 37 | 38      -> printfn "Open quad of %s, loaned from player 3" (tileNotation (4 * x + 3))
          | 42 | 43      -> printfn "Added quad of %s, loaned from player 1" (tileNotation (4 * x + 3))
          | 44 | 46      -> printfn "Added quad of %s, loaned from player 2" (tileNotation (4 * x + 3))
          | 47 | 48      -> printfn "Added quad of %s, loaned from player 3" (tileNotation (4 * x + 3))
          | 52 | 53      -> printfn "Called triplet of %s, loaned from player 1" (tileNotation (4 * x + 3))
          | 54 | 56      -> printfn "Called triplet of %s, loaned from player 2" (tileNotation (4 * x + 3))
          | 57 | 58      -> printfn "Called triplet of %s, loaned from player 3" (tileNotation (4 * x + 3))
          | _ -> failwith "Fatal error"
      ) mt) |> ignore; printfn ""
  let displayDiscardedTiles n =
    assertVaildPlayer n
    match discardedTiles n with
    | 0            -> printfn "None\n"
    | 1            -> printfn "\n%s\n" (tileNotation (Array.findIndex (fun x -> x = (n * 100 + 1)) tiles))
    | x when x > 1 ->
      let dt =
        Array.toList
          (Array.map (fun y -> tileNotation (Array.findIndex (fun z -> z = (n * 100 + y)) tiles)) [| 1..x |])
      printfn "\n%s\n" (List.fold (fun y z -> y + " | " + z) dt.Head dt.Tail)
    | _            -> failwith "Fatal error"

  member __.Init () =
    // Getting unused tiles
    for i in 1..4  do tiles[drawTile false] <- -i
    // Getting bonus tiles
    for i in 1..10 do tiles[drawTile false] <- i
    // Getting initial hand tiles
    for i in 1..39 do tiles[drawTile false] <- 11 + ((i - 1) % 3)
    ()
  
  member __.CheckFirstRound () = firstRound

  member __.IncrementFirstRound () =
    firstRound <- firstRound + 1; ()

  member __.EndFirstRound () =
    firstRound <- 0; ()
  
  member __.CanDrawTile () =
    Array.exists (fun x -> x = 0) tiles

  member __.NumQuads () = numQuads

  member __.IncrementQuad () = numQuads <- numQuads + 1; ()
  
  member __.AbortBy4Quads () =
    (numQuads = 4) && (not (
      List.exists (fun x ->
        Array.length (
          Array.append (getClosedQuads x)
            (Array.append (getOpenOrAddedQuads false x) (getOpenOrAddedQuads true x))
        ) = 4
      ) [ 1..3 ]
    ))
  
  member __.AbortByNineTerminalsAndHonors player =
    assertVaildPlayer player
    if (getDrawnTile player = -1) then (false) else (
      let allTileList = Array.append (getHandTiles player) [| (getDrawnTile player) |]
      List.fold (fun x y ->
        if (Array.exists (fun z -> z / 4 = y) allTileList) then (x + 1) else (x)
      ) 0 [ 0; 1; 2; 10; 11; 19; 20; 21; 22; 23; 24; 25; 26 ] > 8
    )
  
  member __.IsPenalty idx player =
    assertVaildPlayer player
    match discardedTiles player with
    | 0            -> false
    | 1            -> (idx / 4) = ((Array.findIndex (fun x -> x = (player * 100 + 1)) tiles) / 4)
    | x when x > 1 ->
      let dt = Array.map (fun y -> Array.findIndex (fun z -> z = (player * 100 + y)) tiles) [| 1..x |]
      Array.exists (fun x -> x / 4 = idx / 4) dt
    | _            -> failwith "Fatal error"
  
  member __.CallMeld isQuad idx fromWho player =
    assertVaildPlayer player
    let m = idx / 4
    let temp =
      Array.fold (fun x y -> if (y = 10 + player) then (x + 1) else (x)) 0 (tiles[ (4 * m)..(4 * m + 3) ])
    if (isQuad) then (
      // Call for open quad
      if (temp = 3) then (
        for i in (4 * m)..(4 * m + 3) do
          tiles[i] <- 27 + (fromWho * 3) + player
        tileNotation idx
        // incrementing numQuads is done later separately
      ) else (failwith "Invalid action")
    ) else (
      // Call for called triplet
      if (temp = 2) then (
        for i in (4 * m)..(4 * m + 3) do
          if ((i = idx) || (tiles[i] = 10 + player)) then (tiles[i] <- 47 + (fromWho * 3) + player)
        tileNotation idx
      ) else (failwith "Invalid action")
    )
  
  member __.TryMakeMeld player =
    assertVaildPlayer player
    let allTilesList =
      Array.toList (
        Array.map
          (fun (x, _) -> x)
          (Array.filter (fun (_, y) -> ((y = (10 + player)) || (y = (13 + player)))) (Array.indexed tiles))
      )
    let calledTriplets = getCalledTriplets player
    let rec tryHelp prev cnt list =
      match list with
      | hd :: tl ->
        if (hd / 4 = prev) then (
          if (cnt = 3) then (
            // Closed quad possible
            match tl with
            | nhd :: ntl ->
              List.append [ (false, prev, tileNotation hd) ] (tryHelp nhd 1 ntl)
            | _ -> [ (false, prev, tileNotation hd) ]
          ) else (
            tryHelp prev (cnt + 1) tl
          )
        ) else (
          // Check for added quad
          if (Array.exists (fun (t, _) -> t = hd / 4) calledTriplets) then (
            List.append [ (true, hd, tileNotation hd) ] (tryHelp (hd / 4) 1 tl)
          ) else (tryHelp (hd / 4) 1 tl)
        )
      | _ -> []
    tryHelp (allTilesList.Head / 4) 1 allTilesList.Tail
  
  member __.MakeMeld optionInfo player =
    assertVaildPlayer player
    let (isAddedQuad, idx, _) = optionInfo
    if (isAddedQuad) then (
      // idx = 0 ~ 107
      let m = idx / 4
      let (na, nb) =
        match player with
        | 1 -> (54, 57)
        | 2 -> (52, 58)
        | 3 -> (53, 56)
        | _ -> failwith "Fatal error"
      let nx =
        let temp1 =
          Array.fold (fun x y -> if (y = na) then (x + 1) else (x)) 0 (tiles[ (4 * m)..(4 * m + 3) ])
        let temp2 =
          Array.fold (fun x y -> if (y = nb) then (x + 1) else (x)) 0 (tiles[ (4 * m)..(4 * m + 3) ])
        match (temp1, temp2) with
        | (3, _) -> na
        | (_, 3) -> nb
        | (_, _) -> failwith "Invalid action"
      for i in (4 * m)..(4 * m + 3) do tiles[i] <- (nx - 10)
      (idx, tileNotation idx)   // incrementing numQuads is done later separately
    ) else (
      // idx = 0 ~ 26
      if (
        Array.exists
          (fun x -> not ((x = 10 + player) || (x = 13 + player))) (tiles[ (4 * idx)..(4 * idx + 3) ])
      ) then (failwith "Invalid action") else (
        for i in (4 * idx)..(4 * idx + 3) do tiles[i] <- 20 + player
        ((4 * idx + 3), tileNotation (4 * idx + 3))   // incrementing numQuads is done later separately
      )
    )

  member __.DrawTile fromUnused player =
    assertVaildPlayer player
    if (fromUnused) then (
      let drawnTile = drawTile true
      tiles[drawnTile] <- 13 + player
      for i in 2..4 do
        tiles[Array.findIndex (fun x -> x = -i) tiles] <- (1 - i)
      let refillTile = drawTile false
      tiles[refillTile] <- -4
      tileNotation drawnTile
    ) else (
      let drawnTile = drawTile false
      tiles[drawnTile] <- 13 + player
      tileNotation drawnTile
    )
  
  member __.SetTile player =
    assertVaildPlayer player
    if (Array.contains (13 + player) tiles) then (
      tiles[Array.findIndex (fun x -> x = 13 + player) tiles] <- 10 + player
    )
    ()
  
  member __.PutAside4zTile player =
    assertVaildPlayer player
    if (Array.exists (fun x -> (x = 10 + player) || (x = 13 + player)) tiles[ 92..95 ]) then (
      let idx = 92 + (Array.findIndex (fun x -> (x = 10 + player) || (x = 13 + player)) tiles[ 92..95 ])
      tiles[idx] <- 16 + player; idx
    ) else (failwith "Invalid action")
  
  member __.DiscardTile idx player =
    assertVaildPlayer player
    if (not ((tiles[idx] = 10 + player) || (tiles[idx] = 13 + player))) then (failwith "Invalid action")
    tiles[idx] <- player * 100 + (discardedTiles player) + 1
    (idx, tileNotation idx)

  member __.ConstructHand viewPoint =
    (
      getHandTiles viewPoint,
      getMeldTiles viewPoint,
      getDrawnTile viewPoint
    )
  
  member __.BonusTiles () =
    (
      List.map (fun x -> Array.findIndex (fun y -> y = x) tiles) [ 1..(1 + numQuads) ],
      List.map (fun x -> Array.findIndex (fun y -> y = x) tiles) [ 6..(6 + numQuads) ]
    )

  member __.Display viewPoint =
    printfn "===================================================================\n"
    match viewPoint with
    | 0 -> // All-seeing observer perspective\
      printfn "Unused tiles:"
      let unusedTiles = List.map (fun x -> Array.findIndex (fun y -> y = x) tiles) [ -1; -2; -3; -4 ]
      //   Array.toList
      //     (Array.map (fun (x, _) -> x) (Array.filter (fun (_, y) -> y = -1) (Array.indexed tiles)))
      if (List.length unusedTiles > 0) then (
        printfn "%s\n" (
          List.fold (fun x y -> x + " | " + (tileNotation y)) (tileNotation unusedTiles.Head) unusedTiles.Tail
        )
      ) else (printfn "None\n")
      displayBonusTiles 0
      for i in 1..3 do
        printfn "[Player %d]\n" i
        printfn "In hand:"
        displayHandTiles i false
        printf "Melds made: "
        displayMeldTiles i
        printfn "North (4z) tiles put aside: %d\n"
          (Array.fold (fun x y -> if (y = 16 + i) then (x + 1) else (x)) 0 tiles[ 92..95 ])
        printf "Discarded:  "
        displayDiscardedTiles i
      // TBA - How to display open tiles
      printfn "%d tiles remaining in the stack\n" (remainingTiles ())
    | 1 | 2 | 3 ->
      displayBonusTiles (1 + numQuads)
      printfn "[You]\n"
      printfn "In your hand:"
      displayHandTiles viewPoint true
      printf "Melds made: "
      displayMeldTiles viewPoint
      printfn "North (4z) tiles put aside: %d\n"
        (Array.fold (fun x y -> if (y = 16 + viewPoint) then (x + 1) else (x)) 0 tiles[ 92..95 ])
      printf "Discarded:  "
      displayDiscardedTiles viewPoint
      for i in 1..3 do
        if (not (i = viewPoint)) then (
          printfn "[Player %d]\n" i
          printf "Melds made: "
          displayMeldTiles i
          printfn "North (4z) tiles put aside: %d\n"
            (Array.fold (fun x y -> if (y = 16 + i) then (x + 1) else (x)) 0 tiles[ 92..95 ])
          printf "Discarded:  "
          displayDiscardedTiles i
        )
      printfn "%d tiles remaining in the stack\n" (remainingTiles ())
    | _ -> failwith "Fatal error"

  member __.DisplayWin (arg1: int list) (arg2: (int * int) list) arg3 =
    printf "%s" (tileNotation arg1.Head)
    List.map (fun x -> printf " | %s" (tileNotation x)) arg1.Tail |> ignore
    List.map (
      fun (x, y) ->
        if (List.contains y [ 21; 22; 23; 32; 33; 34; 36; 37; 38; 42; 43; 44; 46; 47; 48 ])
          then (
            printf " | [ "
            for i in [ 0..3 ] do printf "%s " (tileNotation (4 * x + i))
            printf "]"
          )
          else (
            let exclude =
              Array.findIndex (
                fun x -> not (List.contains x [ 52; 53; 54; 56; 57; 58 ])
              ) tiles[ (4 * x)..(4 * x + 3) ]
            printf " | [ "
            for i in [ 0..3 ] do
              if (not (i = exclude)) then (printf "%s " (tileNotation (4 * x + i)))
            printf "]"
          )
    ) arg2 |> ignore
    printfn " | %s\n" (tileNotation arg3)
  
  // TBA - More methods to change and/or show game status