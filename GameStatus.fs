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
  //  30 + k   : open   quad    by player (k % 3), loaned from player (k / 3)
  //  40 + k   : added  quad    by player (k % 3), loaned from player (k / 3)
  //  50 + k   : called triplet by player (k % 3), loaned from player (k / 3)
  // 101 ~ 154 : discarded in front of player 1, numbered in order
  // 201 ~ 254 : discarded in front of player 2, numbered in order
  // 301 ~ 354 : discarded in front of player 3, numbered in order
  let tiles = Array.create 108 0
  // for bonus tile display &  four quads abort
  let mutable numQuads = 0
  // for nine different terminal and honor tiles abort
  // and "Blessing of Heaven" & "Blessing of Earth" limit hands
  let mutable firstRound = 1

  // for tile draws
  let rand = System.Random ()
  let rec drawTile fromUnused =
    if (fromUnused) then (Array.findIndex ((=) -1) tiles) else (
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
    |  6 -> if (idx % 4 = 3) then ("5P") else ("5p") // Red tile bonus
    |  7 -> "6p"
    |  8 -> "7p"
    |  9 -> "8p"
    | 10 -> "9p"
    | 11 -> "1s"
    | 12 -> "2s"
    | 13 -> "3s"
    | 14 -> "4s"
    | 15 -> if (idx % 4 = 3) then ("5S") else ("5s") // Red tile bonus
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
  let filteredTiles cond   = Array.filter cond (Array.indexed tiles)
  let countTiles cond list = Array.fold (fun x y -> if (cond y) then (x + 1) else (x)) 0 list
  let remainingTiles () = countTiles ((=) 0) tiles
  let discardedTiles n =
    assertVaildPlayer n; countTiles (fun y -> (y > n * 100) && (y < (n * 100 + 55))) tiles
  let getHandTiles n =
    assertVaildPlayer n; Array.map (fun (x, _) -> x) (filteredTiles (fun (_, y) -> y = (10 + n)))
  let getClosedQuads n =
    if (Array.contains (20 + n) tiles) then (
      Array.map (fun (x, _) -> (x / 4, 20 + n))
        (filteredTiles (fun (x, y) -> (x % 4 = 3) && (y = (20 + n))))
    ) else ( [| |] )
  let getOpenOrAddedQuads n =
    let (na, nb) =
      match n with
      | 1 -> (34, 37)
      | 2 -> (32, 38)
      | 3 -> (33, 36)
      | _ -> failwith "Fatal error"
    let opens =
      Array.append
        (Array.map (fun (x, y) -> (x / 4, y)) (filteredTiles (fun (x, y) -> (x % 4 = 3) && (y = na))))
        (Array.map (fun (x, y) -> (x / 4, y)) (filteredTiles (fun (x, y) -> (x % 4 = 3) && (y = nb))))
    let adds =
      Array.append
        (Array.map (fun (x, y) -> (x / 4, y)) (filteredTiles (fun (x, y) -> (x % 4 = 3) && (y = (na + 10)))))
        (Array.map (fun (x, y) -> (x / 4, y)) (filteredTiles (fun (x, y) -> (x % 4 = 3) && (y = (nb + 10)))))
    Array.append opens adds
  let getCalledTriplets n =
    let (na, nb) =
      match n with
      | 1 -> (54, 57)
      | 2 -> (52, 58)
      | 3 -> (53, 56)
      | _ -> failwith "Fatal error"
    let ta = Array.map (fun (x, _) -> x) (filteredTiles (fun (_, y) -> y = na))
    let tb = Array.map (fun (x, _) -> x) (filteredTiles (fun (_, y) -> y = nb))
    if ((not ((Array.length ta) % 3 = 0)) || (not ((Array.length ta) % 3 = 0))) then (
      failwith "Fatal error"
    ) else (
      Array.append
        (Array.map (fun (_, y) -> (y / 4, na)) (Array.filter (fun (x, _) -> x % 3 = 2) (Array.indexed ta)))
        (Array.map (fun (_, y) -> (y / 4, nb)) (Array.filter (fun (x, _) -> x % 3 = 2) (Array.indexed tb)))
    )
  let getMeldTiles n =
    Array.toList
      (Array.append (Array.append (getClosedQuads n) (getOpenOrAddedQuads n)) (getCalledTriplets n))
  let getDrawnTile n =
    match Array.tryFindIndex ((=) (13 + n)) tiles with
    | Some x ->  x
    | None   -> -1
  let getDiscardedTiles n =
    match discardedTiles n with
    | 0 -> []
    | x -> List.map (fun x -> Array.findIndex ((=) (n * 100 + x)) tiles) [ 1..x ]
  let displayTiles list =
    if (List.length list > 0) then (List.fold (fun x y -> x + " | " + y) list.Head list.Tail) else ("")
  let displayBonusTiles n =
    printfn "Bonus tiles:"
    match n with
    | 0 ->
      printfn "%s"
        (displayTiles (List.map (fun x -> tileNotation (Array.findIndex ((=) x) tiles)) [ 1..5 ]))
      printfn "%s\n"
        (displayTiles (List.map (fun x -> tileNotation (Array.findIndex ((=) x) tiles)) [ 6..10 ]))
    | 1 | 2 | 3 | 4 | 5 ->
      printfn "%s\n" (
        displayTiles (
          [ 1..5 ] |> List.map
            (fun x -> if (x > n) then ("??") else (tileNotation (Array.findIndex ((=) x) tiles)))
        )
      )
    | _  -> failwith "Fatal error"
  let displayHandTiles n index =
    assertVaildPlayer n
    let (ht, dt) = (Array.toList (Array.map tileNotation (getHandTiles n)), getDrawnTile n)
    match dt with
    | -1 ->
      printfn "%s" (displayTiles ht)
      if (index) then (
        for i in 1..(List.length ht) do printf "%2d   " i
        printfn "\n"
      ) else (printfn "")
    |  y -> 
      printfn "%s | %s" (displayTiles ht) (tileNotation y)
      if (index) then (
        for i in 1..(List.length ht) do printf "%2d   " i
        printfn "14\n"
      ) else (printfn "")
  let displayMeldTiles n =
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
    let tl = getDiscardedTiles n
    if (List.length tl = 0) then (printfn "None\n")
      else (printfn "\n%s\n" (displayTiles (List.map (fun x -> tileNotation x) tl)))
  let count4zTile n = countTiles ((=) (16 + n)) tiles[ 92..95 ]

  member __.Init () =
    for i in 1..4  do tiles[drawTile false] <- -i                   // Getting unused tiles
    for i in 1..10 do tiles[drawTile false] <- i                    // Getting bonus tiles
    for i in 1..39 do tiles[drawTile false] <- 11 + ((i - 1) % 3)   // Getting initial hand tiles
    ()
  
  member __.CheckFirstRound () = firstRound

  member __.IncrementFirstRound () = firstRound <- firstRound + 1; ()

  member __.EndFirstRound () = firstRound <- 0; ()
  
  member __.CanDrawTile () = Array.exists ((=) 0) tiles

  member __.NumQuads () = numQuads

  member __.IncrementQuad () = numQuads <- numQuads + 1; ()
  
  member __.AbortBy4Quads () =
    (numQuads = 4) && (not (
      List.exists
        (fun x -> Array.length (Array.append (getClosedQuads x) (getOpenOrAddedQuads x)) = 4) [ 1..3 ]
    ))
  
  member __.AbortByNineTerminalsAndHonors player =
    assertVaildPlayer player
    if (getDrawnTile player = -1) then (-1) else (
      let allTileList = Array.append (getHandTiles player) [| (getDrawnTile player) |]
      countTiles (fun x -> Array.exists (fun y -> (y / 4) = x) allTileList)
        [| 0; 1; 2; 10; 11; 19; 20; 21; 22; 23; 24; 25; 26 |]
    )
  
  member __.IsPenalty waitList player =
    assertVaildPlayer player
    let dt = discardedTiles player
    if (dt > 0) then (
      let dtl = List.map (fun y -> Array.findIndex ((=) (player * 100 + y)) tiles) [ 1..dt ]
      List.exists (fun x -> List.exists (fun y -> (y / 4) = x) dtl) waitList
    ) else (false)
  
  member __.CallMeld isQuad idx fromWho player =
    assertVaildPlayer player
    let m = idx / 4
    let temp = countTiles ((=) (10 + player)) tiles[ (4 * m)..(4 * m + 3) ]
    if (isQuad) then (   // Call for open quad
      if (temp = 3) then (
        for i in (4 * m)..(4 * m + 3) do
          tiles[i] <- 27 + (fromWho * 3) + player
        tileNotation idx   // incrementing numQuads is done later separately
      ) else (failwith "Invalid action")
    ) else (   // Call for called triplet
      if (temp > 1) then (
        let cnt =
          List.fold (
            fun x y ->
              if   (y = idx)                then (tiles[y] <- 47 + (fromWho * 3) + player; x)
              elif (x = 2)                  then (2)
              elif (tiles[y] = 10 + player) then (tiles[y] <- 47 + (fromWho * 3) + player; x + 1)
              else                               (x)
          ) 0 [ for i in 0..3 -> (4 * m + 3 - i) ]
        if (cnt = 2) then (tileNotation idx) else (failwith "Fatal error")
      ) else (failwith "Invalid action")
    )
  
  member __.TryMakeMeld player =
    assertVaildPlayer player
    let allTilesList =
      Array.toList (
        Array.map
          (fun (x, _) -> x) (filteredTiles (fun (_, y) -> ((y = (10 + player)) || (y = (13 + player)))))
      )
    let calledTriplets = getCalledTriplets player
    let rec tryHelp list =
      List.fold (
        fun (prev, cnt, out) y ->
          if (y / 4 = prev) then (   // Closed quad possible
            if (cnt = 3) then (-1, 0, List.append out [ (false, prev, tileNotation y) ])
              else (prev, cnt + 1, out)
          ) else (   // Check for added quad
            if (Array.exists (fun (t, _) -> t = y / 4) calledTriplets)
              then (-1, 0, List.append out [ (true, y, tileNotation y) ]) else (y / 4, 1, out)
          )
      ) (-1, 0, [ ]) list
    let (_, _, temp) = tryHelp allTilesList
    temp   // For debug: printfn "%A / %A" calledTriplets temp
  
  member __.MakeMeld optionInfo player =
    assertVaildPlayer player
    let (isAddedQuad, idx, _) = optionInfo
    if (isAddedQuad) then (   // idx = 0 ~ 107
      let m = idx / 4
      let (na, nb) =
        match player with
        | 1 -> (54, 57)
        | 2 -> (52, 58)
        | 3 -> (53, 56)
        | _ -> failwith "Fatal error"
      let nx =
        let temp1 = countTiles ((=) na) tiles[ (4 * m)..(4 * m + 3) ]
        let temp2 = countTiles ((=) nb) tiles[ (4 * m)..(4 * m + 3) ]
        match (temp1, temp2) with
        | (3, _) -> na
        | (_, 3) -> nb
        | (_, _) -> failwith "Invalid action"
      for i in (4 * m)..(4 * m + 3) do tiles[i] <- (nx - 10)
      (idx, tileNotation idx)   // incrementing numQuads is done later separately
    ) else (   // idx = 0 ~ 26
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
      for i in 2..4 do tiles[Array.findIndex ((=) -i) tiles] <- (1 - i)
      let refillTile = drawTile false
      tiles[refillTile] <- -4; tileNotation drawnTile
    ) else (
      let drawnTile = drawTile false
      tiles[drawnTile] <- 13 + player; tileNotation drawnTile
    )
  
  member __.SetTile player =
    assertVaildPlayer player
    match Array.tryFindIndex ((=) (13 + player)) tiles with
    | Some x -> tiles[x] <- (10 + player); ()
    | None   ->                            ()
  
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
    (getHandTiles viewPoint, getMeldTiles viewPoint, getDrawnTile viewPoint)
  
  member __.GetDisclosedTiles viewPoint =
    List.map (
      fun x ->
        countTiles
          (fun y -> (y = 10 + viewPoint) || (y = 13 + viewPoint) || (y > 16))
          (tiles[ (4 * x)..(4 * x + 3) ])
    ) [ 0..26 ]

  member __.GetDiscardedTiles viewPoint =
    let (n1,  n2)  = (viewPoint % 3 + 1, (viewPoint + 1) % 3 + 1)
    let (tl1, tl2) = (getDiscardedTiles n1, getDiscardedTiles n2)
    List.map (
      fun x ->
        let (tmp1, tmp2) = (List.exists (fun y -> y / 4 = x) tl1, List.exists (fun y -> y / 4 = x) tl2)
        match (tmp1, tmp2) with
        | (true,  true)                  -> 2
        | (true,  false) | (false, true) -> 1
        | (false, false)                 -> 0
    ) [ 0..26 ]
  
  member __.BonusTiles () =
    (
      List.map (fun x -> Array.findIndex ((=) x) tiles) [ 1..(1 + numQuads) ],
      List.map (fun x -> Array.findIndex ((=) x) tiles) [ 6..(6 + numQuads) ]
    )
  
  member __.RedAndNorthBonus player =
    let matchList =
      match player with
      | 1 -> [ 11; 14; 17; 21; 34; 37; 44; 47; 54; 57 ]
      | 2 -> [ 12; 15; 18; 22; 32; 38; 42; 48; 52; 58 ]
      | 3 -> [ 13; 16; 19; 23; 33; 36; 43; 46; 53; 56 ]
      | _ -> failwith "Fatal error"
    match (List.contains tiles[27] matchList, List.contains tiles[63] matchList) with
    | (true, true) -> (2, count4zTile player)
    | (true, false) | (false, true) -> (1, count4zTile player)
    | (false, false) -> (0, count4zTile player)

  member __.DisplayTiles list = displayTiles (List.map (fun x -> tileNotation x) list)

  member __.Display viewPoint readyList =
    printfn "===================================================================\n"
    match viewPoint with
    | 0 ->   // All-seeing observer perspective
      printfn "Unused tiles:"
      let unusedT = List.map (fun x -> Array.findIndex ((=) x) tiles) [ -1; -2; -3; -4 ]
      if (List.length unusedT > 0) then (
        printfn "%s\n" (
          List.fold (fun x y -> x + " | " + (tileNotation y)) (tileNotation unusedT.Head) unusedT.Tail
        )
      ) else (printfn "None\n")
      displayBonusTiles 0
      for i in 1..3 do
        match (List.item (i - 1) readyList) with
        | 2 -> printfn "[Player %d] (Double Ready)\n" i
        | 1 -> printfn "[Player %d] (Ready)\n" i
        | _ -> printfn "[Player %d]\n" i
        printfn "In hand:";    displayHandTiles i false
        printf "Melds made: "; displayMeldTiles i
        printfn "North (4z) tiles put aside: %d\n" (count4zTile i)
        printf "Discarded:  "; displayDiscardedTiles i
      printfn "%d tiles remaining in the stack\n" (remainingTiles ())
      printfn "===================================================================\n"
    | 1 | 2 | 3 ->
      displayBonusTiles (1 + numQuads)
      match (List.item (viewPoint - 1) readyList) with
      | 2 -> printfn "[You] (Double Ready)\n"
      | 1 -> printfn "[You] (Ready)\n"
      | _ -> printfn "[You]\n"
      printfn "In your hand:"; displayHandTiles viewPoint true
      printf "Melds made: ";   displayMeldTiles viewPoint
      printfn "North (4z) tiles put aside: %d\n" (count4zTile viewPoint)
      printf "Discarded:  ";   displayDiscardedTiles viewPoint
      for i in [ ((viewPoint % 3) + 1); ((viewPoint + 1) % 3 + 1) ] do
        match (List.item (i - 1) readyList) with
        | 2 -> printfn "[Player %d] (Double Ready)\n" i
        | 1 -> printfn "[Player %d] (Ready)\n" i
        | _ -> printfn "[Player %d]\n" i
        printf "Melds made: "; displayMeldTiles i
        printfn "North (4z) tiles put aside: %d\n" (count4zTile i)
        printf "Discarded:  "; displayDiscardedTiles i
      printfn "%d tiles remaining in the stack\n" (remainingTiles ())
      printfn "===================================================================\n"
    | _ -> failwith "Fatal error"

  member __.DisplayWin (arg1: int list) (arg2: (int * int) list) arg3 arg7 (bonus1, bonus2) =
    printf "%s" (displayTiles (List.map tileNotation arg1))
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
    printfn "Bonus tiles:\n[ Open ] %s" (displayTiles (List.map tileNotation bonus1))
    if (arg7 > 0) then (
      printfn "[Closed] %s\n" (displayTiles (List.map tileNotation bonus2))
    ) else (printfn "")
      