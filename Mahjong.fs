namespace TermProject

type Mahjong (userPos) =
  let status = GameStatus ()
  let hands  = WinningHands ()
  let players = [| for i in 1..3 -> if (i = userPos) then (Player (true)) else (Player (false)) |]
    // TBA - Add further information in players list

  member __.Run () =
    let rec mainLoop wasQuad shouldIncrementQuad turn =
      let handleWin infoList =
        let printResult isLimit points handList =
          List.map (fun (p, n) -> printfn "%s:\n%d points\n" n p) handList |> ignore
          if (isLimit) then (
            match points with
            | 1 -> printfn "Limit hand!\n"
            | 2 -> printfn "Double Limit hand!\n"
            | 3 -> printfn "Triple Limit hand!\n"
            | 4 -> printfn "Quadruple Limit hand!\n"
            | 5 -> printfn "Quintuple Limit hand!\n"
            | 6 -> printfn "Sextuple Limit hand!\n"
            | _ -> failwith "Fatal error"
          ) else (
            if (points > 12)
              then (printfn "Counted limit Hand - %d points!\n" points)
              else (printfn "Normal hand - %d points!\n" points)
          )

        let bonusTiles = status.BonusTiles ()
        match List.length infoList with
        | 1 -> 
          let (isLimit, points, handList) =
            hands.GetPoints (infoList.Head) bonusTiles
          let (n, arg1, arg2, arg3, _, _, _, _, _, _) = infoList.Head
          printfn "Player %d has won!\n" n
          status.DisplayWin arg1 arg2 arg3
          printResult isLimit points handList
        | 2 ->
          let (isLimit1, points1, handList1) =
            hands.GetPoints (infoList.Head) bonusTiles
          let (n1, n1arg1, n1arg2, n1arg3, _, _, _, _, _, _) = infoList.Head
          let (isLimit2, points2, handList2) =
            hands.GetPoints (infoList.Tail.Head) bonusTiles
          let (n2, n2arg1, n2arg2, n2arg3, _, _, _, _, _, _) = infoList.Tail.Head
          printfn "Player %d and %d has both won!\n" n1 n2
          status.DisplayWin n1arg1 n1arg2 n1arg3   // temporary
          printResult isLimit1 points1 handList1
          status.DisplayWin n2arg1 n2arg2 n2arg3   // temporary
          printResult isLimit2 points2 handList2
        | _ -> failwith "Fatal error"
        ()

      let rec handleReaction actionPlayer currTile actionType keepTurn =
        // actionType: 1 - closed quad, 2 - added quad, 3 - North (4z) put aside, 0 - other
        let handleNextTurn wasQuad incrementQuadNext nextPlayer =
          // check for no winning hand abort & call this player
          match (status.AbortBy4Quads (), status.CanDrawTile ()) with
          | (true,  _)     ->
            printfn "Abort by 4 quads\n"
            ()
          | (false, true)  ->
            mainLoop wasQuad incrementQuadNext nextPlayer
          | (false, false) ->
            printfn "No player was able to complete a winning hand.\n"
            ()
        
        let handleQuad n =
          let meldedTile = status.CallMeld true currTile actionPlayer n
          status.EndFirstRound ()
          printfn "Player %d has called for an open quad: %s\n" n meldedTile; handleNextTurn true true n
        
        let handleTri n =
          let meldedTile = status.CallMeld false currTile actionPlayer n
          status.EndFirstRound ()
          printfn "Player %d has called for a triplet: %s\n" n meldedTile
          let (newHand, _, _) = status.ConstructHand n
          if (players[n - 1].IsUser ()) then (status.Display n)
          let nextMove = players[n - 1].DoDiscard newHand
          let (discardedTile, discadedTileName) = status.DiscardTile newHand[nextMove] n
          printfn "Player %d has discarded: %s\n" n discadedTileName
          handleReaction n discardedTile 0 false

        let (np1, np2) = ((actionPlayer % 3) + 1, ((actionPlayer + 1) % 3) + 1)
        let handInfo1 =
          let (arg1, arg2, _) = status.ConstructHand np1
          (
            np1, (Array.toList arg1), arg2, currTile, false, false, actionType = 2,
            players[np1 - 1].IsReady (), false, not (status.CanDrawTile ())
          )
        let handInfo2 =
          let (arg1, arg2, _) = status.ConstructHand np2
          (
            np2, (Array.toList arg1), arg2, currTile, false, false, actionType = 2,
            players[np2 - 1].IsReady (), false, not (status.CanDrawTile ())
          )
        if (actionType = 1) then (
          // TBA - check if someone is waiting for "Thirteen orphans"
          let testList = [ "Thirteen Orphans | 13-tile Wait"; "Thirteen Orphans" ]
          let win1 =
            if (
              (List.exists (fun x -> hands.IsThisHand x handInfo1) testList)
                && (not (status.IsPenalty currTile np1)) && (not (players[np1 - 1].HasTempPenalty ()))
            ) then (
              let choice = players[np1 - 1].DoWin ()
              // TBA - should feed handInfo as argument for bot information
              if (not choice) then (players[np1 - 1].GivePenalty (); false) else (true)
            ) else (false)
          let win2 =
            if (
              (not win1) && (List.exists (fun x -> hands.IsThisHand x handInfo2) testList)
                && (not (status.IsPenalty currTile np2)) && (not (players[np2 - 1].HasTempPenalty ()))
            ) then (
              let choice = players[np2 - 1].DoWin ()
              // TBA - should feed handInfo as argument for bot information
              if (not choice) then (players[np2 - 1].GivePenalty (); false) else (true)
            ) else (false)
          match (win1, win2) with
          | (true,  true)  -> handleWin [ handInfo1; handInfo2 ]
          | (true,  false) -> handleWin [ handInfo1 ]
          | (false, true)  -> handleWin [ handInfo2 ]
          | (false, false) ->
            status.IncrementQuad ()
            handleNextTurn true false actionPlayer
        ) else (
          // check if someone is waiting for any winning tile
          let win1 =
            if (
              (hands.IsWinning handInfo1) && (not (status.IsPenalty currTile np1))
                && (not (players[np1 - 1].HasTempPenalty ()))
            ) then (
              let choice = players[np1 - 1].DoWin ()
              // TBA - should feed handInfo as argument for bot information
              if (not choice) then (players[np1 - 1].GivePenalty (); false) else (true)
            ) else (false)
          let win2 =
            if (
              (not win1) && (hands.IsWinning handInfo2) && (not (status.IsPenalty currTile np2))
                && (not (players[np2 - 1].HasTempPenalty ()))
            ) then (
              let choice = players[np2 - 1].DoWin ()
              // TBA - should feed handInfo as argument for bot information
              if (not choice) then (players[np2 - 1].GivePenalty (); false) else (true)
            ) else (false)
          let countTile handInfo =
            let (_, arg1, _, _, _, _, _, _, _, _) = handInfo
            List.fold (fun x y -> if (y / 4 = currTile / 4) then (x + 1) else (x)) 0 arg1
          let nr1 = (players[np1 - 1].IsReady () = 0)
          let nr2 = (players[np2 - 1].IsReady () = 0)
          let quad1 =
            if (
              (not (win1 || win2)) && (not keepTurn) && (nr1) && (countTile handInfo1 = 3)
            ) then (
              players[np1 - 1].DoQuad ()
              // TBA - should feed handInfo as argument for bot information
            ) else (false)
          let quad2 =
            if (
              (not (win1 || win2 || quad1)) && (not keepTurn) && (nr2) && (countTile handInfo2 = 3)
            ) then (
              players[np2 - 1].DoQuad ()
              // TBA - should feed handInfo as argument for bot information
            ) else (false)
          let tri1 =
            if (
              (not (win1 || win2 || quad1 || quad2)) && (not keepTurn)
                && (nr1) && (countTile handInfo1 = 2)
            ) then (
              players[np1 - 1].DoTri ()
              // TBA - should feed handInfo as argument for bot information
            ) else (false)
          let tri2 =
            if (
              (not (win1 || win2 || quad1 || quad2 || tri1)) && (not keepTurn)
                && (nr2) && (countTile handInfo2 = 2)
            ) then (
              players[np2 - 1].DoTri ()
              // TBA - should feed handInfo as argument for bot information
            ) else (false)
          match (win1, win2, quad1, quad2, tri1, tri2) with
          | (true,  true,  _,     _,     _,     _)     ->
            handleWin [ handInfo1; handInfo2 ]
          | (true,  false, _,     _,     _,     _)     -> handleWin [ handInfo1 ]
          | (false, true,  _,     _,     _,     _)     -> handleWin [ handInfo2 ]
          | (false, false, true,  _,     _,     _)     -> handleQuad np1
          | (false, false, false, true,  _,     _)     -> handleQuad np2
          | (false, false, false, false, true,  _)     -> handleTri  np1
          | (false, false, false, false, false, true)  -> handleTri  np2
          | (false, false, false, false, false, false) ->
            handleNextTurn keepTurn (actionType = 2) (if (keepTurn) then (actionPlayer) else (np1))
        )

      let isFirstRound =
        match (status.CheckFirstRound (), turn) with
        | (0, _) -> false
        | (1, 1) -> true
        | (1, 2) -> status.IncrementFirstRound (); true
        | (2, 2) -> true
        | (2, 3) -> status.IncrementFirstRound (); true
        | (3, 3) -> true
        | (_, _) -> status.EndFirstRound (); false
      let drawnTile = status.DrawTile wasQuad turn   // draw tile
      // printfn "Player %d has drawn: %s\n" turn drawnTile   // For debugging
      if (players[turn - 1].HasTempPenalty ()) then (players[turn - 1].RemovePenalty ();)
      let (arg1x, arg2, arg3) = status.ConstructHand turn
      let arg1 = Array.toList arg1x
      let arg9 = not (status.CanDrawTile ())
      let handInfo =
        (
          turn, arg1, arg2, arg3, true, isFirstRound, wasQuad,
          players[turn - 1].IsReady (), false, arg9   // TBA - arg8 (inOneTurn)
        )
      let isWinning = hands.IsWinning handInfo
      let isReady   = players[turn - 1].IsReady () > 0
      let readyOpt  = hands.CanDeclareReady handInfo
      let canReady  =
        (not isReady) && (List.length readyOpt > 0) && (List.forall (fun (_, y) -> y = 20 + turn) arg2)
      let quadOption =
        if (
          ((status.NumQuads () + (if (shouldIncrementQuad) then (1) else (0))) < 4) && (not arg9)
        ) then (
          let temp = status.TryMakeMeld turn
          if (isReady) then (   // TBA - filter cases where the waiting tiles change
            List.filter (fun (x, y, _) -> (x) || (y = arg3 / 4)) temp
          ) else (temp)
        ) else ( [ ] )
      let canPutAside4zTile =
        (not arg9) && ((List.exists (fun x -> x / 4 = 23) arg1) || (arg3 / 4 = 23))
      let canAbort = (isFirstRound) && (status.AbortByNineTerminalsAndHonors turn)
      if (players[turn - 1].IsUser ()) then (
        status.Display turn
        if (isWinning) then (printfn "Winning possible: Enter [15] to claim victory")
        if (canReady) then (
          match List.length readyOpt with
          | 1 -> 
            printfn "Claiming ready possible: Enter [16] to perform action"
            printfn "(Index of tile to discard in case: %d)" (readyOpt.Head + 1)
          | x when x > 1 ->
            printfn "Claiming ready possible: Enter [16] to perform action"
            printf "(Index of tiles to discard in case: %d" (readyOpt.Head + 1)
            for i in readyOpt.Tail do printf ", %d" (i + 1)
            printfn ")"
          | _ -> failwith "Fatal error"
        )
        match List.length quadOption with
        | 0 -> ()
        | x when (x > 0) && (x < 5) -> printfn "Possible to make a quad: Enter [17] to perform action"
        | _ -> failwith "Fatal error"
        if (canPutAside4zTile) then (
          printfn
            "Possible to put aside a North (4z) tile: Enter [18] to perform action"
        )
        if (canAbort) then (
          printfn
            "Possible to abort by nine different terminal and honor tiles: Enter [19] to perform action"
        )
      )
      if (shouldIncrementQuad) then (status.IncrementQuad ())
      let nextMove =
        players[turn - 1].NextMove
          (List.length arg1 - 1) isWinning isReady canReady
          (List.length quadOption > 0) canPutAside4zTile canAbort
        // TBA - should feed handInfo as argument for bot information
      match nextMove with
      | 14 -> handleWin [ handInfo ]
      | 15 ->
        match List.length readyOpt with
        | 1 ->
          let (discardedTile, discadedTileName) =
            if (readyOpt.Head = 13) then (
              status.DiscardTile arg3 turn
            ) else (
              status.DiscardTile arg1[readyOpt.Head] turn
            )
          players[turn - 1].DeclareReady isFirstRound
          printfn "Player %d has discarded: %s" turn discadedTileName
          printfn "Player %d has declared ready!\n" turn
          status.SetTile turn
          handleReaction turn discardedTile 0 false
        | x when (x > 1) ->
          let readySelection =
            readyOpt.Head // **TODO** - get user selection
          let (discardedTile, discadedTileName) =
            if (readySelection = 13) then (
              status.DiscardTile arg3 turn
            ) else (
              status.DiscardTile arg1[readySelection] turn
            )
          players[turn - 1].DeclareReady isFirstRound
          printfn "Player %d has discarded: %s\n" turn discadedTileName
          printfn "Player %d has declared ready!" turn
          status.SetTile turn
          handleReaction turn discardedTile 0 false
        | _ -> failwith "Fatal error"
      | 16 ->
        let ((meldedTile, meldedTileName), isAddedQuad) =
          match List.length quadOption with
          | 1 ->
            let (x, _, _) = quadOption.Head
            (status.MakeMeld quadOption.Head turn, x)
          | x when (x > 1) && (x < 5) ->
            let areAdded    = List.map (fun (x, _, _) -> x) quadOption
            let optionNames = List.map (fun (_, _, z) -> z) quadOption
            if (players[turn - 1].IsUser ()) then (
              printfn "Multiple options available:"
              for i in 0..(x - 1) do
                printfn "[%d] %s %s"
                  (i + 1)
                  (if (List.item i areAdded) then ("(Added) ") else ("(Closed)"))
                  (List.item i optionNames)
            )
            let quadSelection = players[turn - 1].SelectQuadOption x
            // TBA - should feed handInfo as argument for bot information
            (status.MakeMeld (List.item quadSelection quadOption) turn, List.item quadSelection areAdded)
          | _ -> failwith "Fatal error"
        status.EndFirstRound ()
        if (isAddedQuad) then (
          printfn "Player %d has added a tile to their called triplet: %s\n" turn meldedTileName
          status.SetTile turn
          handleReaction turn meldedTile 2 true
        ) else (
          printfn "Player %d has melded 4 tiles into a closed quad: %s\n" turn meldedTileName
          status.SetTile turn
          handleReaction turn meldedTile 1 true
        )
      | 17 ->
        let putTile = status.PutAside4zTile turn
        status.EndFirstRound ()
        printfn "Player %d has put aside a North (4z) tile\n" turn
        status.SetTile turn
        handleReaction turn putTile 3 true
      | 18 ->
        printfn "Player %d has called for abort by nine different terminal and honor tiles\n" turn
        ()
      | x when ((x > -1) && (x < 14)) ->
        let (discardedTile, discadedTileName) =
          if (x = 13) then (
            status.DiscardTile arg3 turn
          ) else (
            status.DiscardTile arg1[nextMove] turn
          )
        printfn "Player %d has discarded: %s\n" turn discadedTileName
        status.SetTile turn
        handleReaction turn discardedTile 0 false
      | _ -> failwith "Fatal error"
    
    let printPlayer i = if (players[i - 1].IsUser ()) then ("User") else ("Bot")
    printfn "Dealer - East  : %s"   (printPlayer 1)
    printfn "Second - South : %s"   (printPlayer 2)
    printfn "Third  - West  : %s\n" (printPlayer 3)
    status.Init ()
    // status.Display 0   // for debugging
    mainLoop false false 1
    // status.Display 0   // for debugging
    printfn "Some functions of the original game is still missing,"
    printfn "but content will be added in due time...\n"
    ()
  
  /// Debug mode
  member __.RunDebug () =
    // WinningHand debugging
    (*
    let testWinningHand handName argIn =
      printfn "Check %s:" handName
      if (hands.IsThisHand handName argIn) then (printfn "OK\n") else (printfn "Error\n")
    printfn "Debugging limit hands...\n"
    testWinningHand "Thirteen Orphans | 13-tile Wait"
      ([| 0; 4; 8; 40; 44; 76; 80; 84; 88; 92; 96; 100; 104 |], [ ], 1, false, false)
    testWinningHand "Pure Nine Gates"
      ([| 8; 9; 10; 12; 16; 20; 24; 28; 32; 36; 40; 41; 42 |], [ ], 11, false, false)
    testWinningHand "Big Four Winds"
      ([| 80; 81; 82; 84; 85; 86; 88; 89; 90; 92; 93; 94; 96 |], [ ], 97, false, false)
    testWinningHand "Four Concealed Triplets | Single Wait"
      ([| 80; 81; 82; 84; 96; 97; 98; 100; 101; 102; 104; 105; 106 |], [ ], 87, false, false)
    testWinningHand "Thirteen Orphans"
      ([| 4; 8; 40; 44; 76; 80; 84; 88; 92; 96; 100; 104; 105 |], [ ], 0, false, false)
    testWinningHand "Nine Gates"
      ([| 44; 45; 48; 52; 56; 60; 64; 68; 72; 76; 77; 78; 79 |], [ ], 47, false, false)
    testWinningHand "All Green"
      ([| 48; 49; 52; 53; 56; 57; 64; 65; 66; 100 |], [ (18, 34) ], 101, false, false)
    testWinningHand "Big Three Dragons"
      ([| 0; 1; 8; 13; 18; 104; 105 |], [ (24, 44); (25, 47) ], 106, false, false)
    testWinningHand "Small Four Winds"
      ([| 80; 81; 82; 84; 85; 86; 88 |], [ (0, 47); (23, 44) ], 91, false, false)
    testWinningHand "All Terminals"
      ([| 0; 1; 2; 4; 5; 6; 8; 9; 10; 40; 41; 42; 44 |], [ ], 45, false, false)
    testWinningHand "All Honors"
      ([| 80; 81; 84; 85; 88; 89; 92; 93; 96; 97; 100; 101; 104 |], [ ], 105, false, false)
    testWinningHand "Four Concealed Triplets"
      ([| 80; 81; 82; 84; 85; 86; 88; 89; 90; 92; 93; 96; 97 |], [ ], 94, true, false)
    testWinningHand "Four Quads"
      ([| 96 |], [ (20, 21); (21, 34); (22, 44); (23, 47) ], 97, false, false)
    printfn "Debugging normal hands...\n"
    testWinningHand "Perfect Flush"
      ([| 48; 49; 52; 53; 56; 57; 60; 64; 65; 68; 69; 72; 73 |], [ ], 61, false, false)
    testWinningHand "All Terminals and Honors"
      ([| 0; 1; 2; 4; 5; 6; 96; 97; 100; 101; 104; 105; 106 |], [ ], 98, false, false)
    testWinningHand "Perfect Ends"
      ([| 6; 8; 9; 12; 13; 16; 17; 32; 33; 36; 37; 40; 41 |], [ ], 7, false, false)
    testWinningHand "Double Identical Sequences"
      ([| 48; 49; 52; 53; 56; 57; 60; 64; 65; 68; 69; 72; 73 |], [ ], 61, false, false)
    testWinningHand "Common Flush"
      ([| 48; 49; 52; 53; 56; 57; 64; 65; 68; 69; 72; 73; 96 |], [ ], 99, false, false)
    testWinningHand "Seven Pairs"
      ([| 12; 13; 16; 17; 20; 21; 24; 28; 29; 32; 33; 36; 37 |], [ ], 25, false, false)
    testWinningHand "Full Straight"
      ([| 0; 3; 44; 48; 52; 56; 64; 68; 72; 76 |], [ (20, 34) ], 63, false, false)
    testWinningHand "Three Mixed Triplets"
      ([| 12; 16; 20; 24; 27; 78; 79 |], [ (1, 34); (10, 37) ], 76, false, false)
    testWinningHand "Common Ends"
      ([| 6; 8; 9; 12; 13; 16; 17; 32; 37; 42 |], [ (24, 34) ], 7, false, false)
    testWinningHand "Little Three Dragons"
      ([| 12; 16; 20; 100; 101; 102; 106 |], [ (1, 34); (24, 37) ], 107, false, false)
    testWinningHand "All Triplets"
      ([| 12; 13; 14; 16; 17; 18; 24; 32; 33; 34; 36; 37; 38 |], [ ], 25, false, false)
    testWinningHand "Three Concealed Triplets"
      ([| 80; 81; 82; 84; 85; 86; 88; 89; 96; 97 |], [ (23, 34) ], 91, true, false)
    testWinningHand "Three Quads"
      ([| 0; 1; 96; 99 |], [ (20, 21); (21, 34); (22, 44) ], 97, false, false)
    *)
    printfn "Debug mode is temporaily disabled"
    ()