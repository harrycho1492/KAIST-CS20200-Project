namespace TermProject

type Mahjong (userPos) =
  let status = GameStatus ()
  let hands  = WinningHands ()
  let players =
    [| for i in 1..3 -> if (i = userPos) then (Player (true, hands)) else (Player (false, hands)) |]
    // TBA - Add further information in players list

  member __.Run () =
    let rec mainLoop wasQuad shouldIncrementQuad turn =
      let handleWin infoList =
        let printResult isLimit points handList =
          List.map (fun (p, n) -> printfn "%s:\n%d points\n" n p) handList |> ignore
          if (isLimit) then (
            match points with
            | 1 -> printfn "Limit hand!\n"
            | 2 -> printfn "Double limit hand!\n"
            | 3 -> printfn "Triple limit hand!\n"
            | 4 -> printfn "Quadruple limit hand!\n"
            | 5 -> printfn "Quintuple limit hand!\n"
            | 6 -> printfn "Sextuple limit hand!\n"
            | _ -> failwith "Fatal error"
          ) else (
            if (points > 12)
              then (printfn "Counted limit Hand - %d points!\n" points)
              else (printfn "Normal hand - %d points!\n" points)
          )

        let bonusTiles = status.BonusTiles ()
        match List.length infoList with
        | 1 -> 
          let (n, arg1, arg2, arg3, arg4, _, _, arg7, _, _) = infoList.Head
          let redAndNorthBonus = status.RedAndNorthBonus n
          let (isLimit, points, handList) =
            hands.GetPoints (infoList.Head) bonusTiles redAndNorthBonus
          printfn "Player %d has won by %s!\n" n (if (arg4) then ("draw") else ("discard"))
          status.DisplayWin arg1 arg2 arg3 arg7 bonusTiles
          printResult isLimit points handList
        | 2 ->
          let (n1, n1arg1, n1arg2, n1arg3, _, _, _, n1arg7, _, _) = infoList.Head
          let redAndNorthBonus1 = status.RedAndNorthBonus n1
          let (isLimit1, points1, handList1) =
            hands.GetPoints (infoList.Head) bonusTiles redAndNorthBonus1
          let (n2, n2arg1, n2arg2, n2arg3, _, _, _, n2arg7, _, _) = infoList.Tail.Head
          let redAndNorthBonus2 = status.RedAndNorthBonus n2
          let (isLimit2, points2, handList2) =
            hands.GetPoints (infoList.Tail.Head) bonusTiles redAndNorthBonus2
          printfn "Player %d and %d has both won by discard!\n" n1 n2
          status.DisplayWin n1arg1 n1arg2 n1arg3 n1arg7 bonusTiles
          printResult isLimit1 points1 handList1
          status.DisplayWin n2arg1 n2arg2 n2arg3 n2arg7 bonusTiles
          printResult isLimit2 points2 handList2
        | _ -> failwith "Fatal error"
        ()

      let rec handleReaction actionPlayer currTile actionType keepTurn =
        // actionType: 1 - closed quad, 2 - added quad, 3 - North (4z) put aside, 0 - other
        let handleNextTurn wasQuad incrementQuadNext nextPlayer =
          match (status.AbortBy4Quads (), status.CanDrawTile ()) with   // check for abort
          | (true,  _)     -> printfn "Abort by 4 quads\n"; ()
          | (false, true)  -> mainLoop wasQuad incrementQuadNext nextPlayer
          | (false, false) -> printfn "No player was able to complete a winning hand.\n"; ()
        
        let handleQuad n =
          for i in 0..2 do players[i].EndOneTurn ()
          let meldedTile = status.CallMeld true currTile actionPlayer n
          status.EndFirstRound ()
          printfn "Player %d has called for an open quad: %s\n" n meldedTile; handleNextTurn true true n
        
        let handleTri n handInfo =
          for i in 0..2 do players[i].EndOneTurn ()
          let meldedTile = status.CallMeld false currTile actionPlayer n
          status.EndFirstRound ()
          printfn "Player %d has called for a triplet: %s\n" n meldedTile
          let (arg0, _, _, _, _, arg5, arg6, arg7, arg8, arg9) = handInfo
          let (newHand, newMeld, _) = status.ConstructHand n
          let newHandInfo = (arg0, Array.toList newHand, newMeld, -1, false, arg5, arg6, arg7, arg8, arg9)
          if (players[n - 1].IsUser ())
            then (status.Display n (Array.toList (Array.map (fun (x: Player) -> x.IsReady ()) players)))
          let nextMove =
            players[n - 1].DoDiscard
              newHand (newHandInfo, (status.BonusTiles ()), (status.GetDisclosedTiles n))
          let (discardedTile, discadedTileName) = status.DiscardTile newHand[nextMove] n
          printfn "Player %d has discarded: %s\n" n discadedTileName
          handleReaction n discardedTile 0 false

        let (np1, np2) = ((actionPlayer % 3) + 1, ((actionPlayer + 1) % 3) + 1)
        let handInfo1 =
          let (arg1, arg2, _) = status.ConstructHand np1
          (
            np1, (Array.toList arg1), arg2, currTile, false, false, actionType = 2,
            players[np1 - 1].IsReady (), players[np1 - 1].InOneTurn (), not (status.CanDrawTile ())
          )
        let waiting1  = hands.IsOneAway handInfo1
        let handInfo2 =
          let (arg1, arg2, _) = status.ConstructHand np2
          (
            np2, (Array.toList arg1), arg2, currTile, false, false, actionType = 2,
            players[np2 - 1].IsReady (), players[np2 - 1].InOneTurn (), not (status.CanDrawTile ())
          )
        let waiting2  = hands.IsOneAway handInfo2
        if (actionType = 1) then (   // check if someone is waiting for "Thirteen orphans"
          for i in 0..2 do players[i].EndOneTurn ()
          let testList = [ "Thirteen Orphans | 13-tile Wait"; "Thirteen Orphans" ]
          let win1 =
            if (
              (List.exists (fun x -> hands.IsThisHand x handInfo1) testList)
                && (not (status.IsPenalty waiting1 np1)) && (not (players[np1 - 1].HasTempPenalty ()))
            ) then (
              let choice = players[np1 - 1].DoWin ()
              // TBA - should feed handInfo as argument for bot information
              if (not choice) then (players[np1 - 1].GivePenalty (); false) else (true)
            ) else (false)
          let win2 =
            if (
              (not win1) && (List.exists (fun x -> hands.IsThisHand x handInfo2) testList)
                && (not (status.IsPenalty waiting2 np2)) && (not (players[np2 - 1].HasTempPenalty ()))
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
          let debugWin npx checks =
            match checks with
            | (false, _,     _)     -> printfn "[Declined: Reason 1]"
            | (true,  false, _)     ->
              printfn "[Declined: Reason 2]"
              if (npx = np1) then (printfn "%A" waiting1) else (printfn "%A" waiting2)
            | (true,  true,  false) -> printfn "[Declined: Reason 3]"
            | (true,  true,  true)  -> printfn "[Should be accepted]"

          let win1 =   // check if someone is waiting for a winning tile
            let (check1, check2, check3) = (
              hands.IsWinning handInfo1,
              not (status.IsPenalty waiting1 np1),
              not (players[np1 - 1].HasTempPenalty ())
            )
            if (check1 && check2 && check3) then (
              let choice = players[np1 - 1].DoWin ()
              // TBA - should feed handInfo as argument for bot information
              if (not choice) then (players[np1 - 1].GivePenalty (); false) else (true)
            ) else (false) // (debugWin np1 (check1, check2, check3); false)
          let win2 =
            let (check1, check2, check3) = (
              hands.IsWinning handInfo2,
              not (status.IsPenalty waiting2 np2),
              not (players[np2 - 1].HasTempPenalty ())
            )
            if (check1 && check2 && check3) then (
              let choice = players[np2 - 1].DoWin ()
              // TBA - should feed handInfo as argument for bot information
              if (not choice) then (players[np2 - 1].GivePenalty (); false) else (true)
            ) else (false) // (debugWin np2 (check1, check2, check3); false)
          let countTile handInfo =
            let (_, arg1, _, _, _, _, _, _, _, _) = handInfo
            List.fold (fun x y -> if (y / 4 = currTile / 4) then (x + 1) else (x)) 0 arg1
          let nr1 = (players[np1 - 1].IsReady () = 0)
          let nr2 = (players[np2 - 1].IsReady () = 0)
          let quad1 =
            if (
              (not (win1 || win2)) && (not keepTurn) && (nr1) && (countTile handInfo1 = 3)
            ) then (
              players[np1 - 1].DoQuad actionPlayer
                (handInfo1, (status.BonusTiles ()), (status.GetDisclosedTiles np1))
            ) else (false)
          let quad2 =
            if (
              (not (win1 || win2 || quad1)) && (not keepTurn) && (nr2) && (countTile handInfo2 = 3)
            ) then (
              players[np2 - 1].DoQuad actionPlayer
                (handInfo2, (status.BonusTiles ()), (status.GetDisclosedTiles np2))
            ) else (false)
          let tri1 =
            if (
              (not (win1 || win2 || quad1 || quad2)) && (not keepTurn)
                && (nr1) && (countTile handInfo1 > 1)
            ) then (
              players[np1 - 1].DoTri actionPlayer
                (handInfo1, (status.BonusTiles ()), (status.GetDisclosedTiles np1))
            ) else (false)
          let tri2 =
            if (
              (not (win1 || win2 || quad1 || quad2 || tri1)) && (not keepTurn)
                && (nr2) && (countTile handInfo2 > 1)
            ) then (
              players[np2 - 1].DoTri actionPlayer
                (handInfo2, (status.BonusTiles ()), (status.GetDisclosedTiles np2))
            ) else (false)
          match (win1, win2, quad1, quad2, tri1, tri2) with
          | (true,  true,  _,     _,     _,     _)     ->
            handleWin [ handInfo1; handInfo2 ]
          | (true,  false, _,     _,     _,     _)     -> handleWin [ handInfo1 ]
          | (false, true,  _,     _,     _,     _)     -> handleWin [ handInfo2 ]
          | (false, false, true,  _,     _,     _)     -> handleQuad np1
          | (false, false, false, true,  _,     _)     -> handleQuad np2
          | (false, false, false, false, true,  _)     -> handleTri  np1 handInfo1
          | (false, false, false, false, false, true)  -> handleTri  np2 handInfo2
          | (false, false, false, false, false, false) ->
            if (keepTurn) then (for i in 0..2 do players[i].EndOneTurn ())
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
          players[turn - 1].IsReady (), players[turn - 1].InOneTurn (), arg9
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
          if (isReady) then (
            List.filter (
              fun (x, y, _) ->
                (x) || ((y = arg3 / 4) && (
                  let newHand =
                    (
                      turn, List.filter (fun x -> not (x / 4 = y)) arg1,
                      List.append arg2 [ (y, 20 + turn) ], -1, true, isFirstRound,
                      wasQuad, players[turn - 1].IsReady (), false, arg9
                    )
                  List.fold2 (fun x y z -> if (y = z) then (x) else (false)) true
                    (hands.IsOneAway handInfo) (hands.IsOneAway newHand)
                ))
            ) temp
          ) else (temp)
        ) else ( [ ] )
      let canPutAside4zTile =
        (not arg9) && ((List.exists (fun x -> x / 4 = 23) arg1) || (arg3 / 4 = 23))
      let canAbort = if (isFirstRound) then (status.AbortByNineTerminalsAndHonors turn) else (-1)
      if (players[turn - 1].IsUser ()) then (
        status.Display turn (Array.toList (Array.map (fun (x: Player) -> x.IsReady ()) players))
        if (isWinning) then (printfn "Winning possible: Enter [15] to claim victory")
        if (canReady) then (
          printfn "Claiming ready possible: Enter [16] to perform action"
          List.map (
            fun (x, l) ->
              printfn "- If tile #%2d is discarded, then the list of waiting tile(s) is:" (x + 1)
              printf  "  %s" (status.DisplayTiles (List.map (fun x -> 4 * x) l))
              if (status.IsPenalty l turn) then (printfn " [Penalty]") else (printfn "")
          ) readyOpt |> ignore
        )
        match List.length quadOption with
        | 0 -> ()
        | x when (x > 0) && (x < 5) -> printfn "Possible to make a quad: Enter [17] to perform action"
        | _ -> failwith "Fatal error"
        if (canPutAside4zTile)
          then (printfn "Possible to put aside a North (4z) tile: Enter [18] to perform action")
        if (canAbort > 8) then (
          printfn
            "Possible to abort by nine different terminal and honor tiles: Enter [19] to perform action"
        )
      )
      if (shouldIncrementQuad) then (status.IncrementQuad ())
      if (players[turn - 1].InOneTurn ()) then (players[turn - 1].EndOneTurn ())
      let nextMove =
        players[turn - 1].NextMove
          (List.length arg1 - 1) isWinning isReady canReady quadOption canPutAside4zTile
          canAbort (handInfo, (status.BonusTiles ()), (status.GetDisclosedTiles turn))
      match nextMove with
      | 14 -> handleWin [ handInfo ]
      | 15 ->
        match List.length readyOpt with
        | 1 ->
          let (target, _) = readyOpt.Head
          let (discardedTile, discadedTileName) =
            if (target = 13) then (status.DiscardTile arg3 turn)
              else (status.DiscardTile arg1[target] turn)
          players[turn - 1].DeclareReady isFirstRound
          printfn "Player %d has discarded: %s" turn discadedTileName
          printfn "Player %d has declared ready!\n" turn
          status.SetTile turn
          handleReaction turn discardedTile 0 false
        | x when (x > 1) ->
          let readySelection =
            players[turn - 1].SelectReadyOption readyOpt (status.GetDisclosedTiles turn)
          let (discardedTile, discadedTileName) =
            if (readySelection = 13) then (status.DiscardTile arg3 turn)
              else (status.DiscardTile arg1[readySelection] turn)
          players[turn - 1].DeclareReady isFirstRound
          printfn "Player %d has discarded: %s" turn discadedTileName
          printfn "Player %d has declared ready!\n" turn
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
                printfn "[%d] %s %s" (i + 1)
                  (if (List.item i areAdded) then ("(Added) ") else ("(Closed)"))
                  (List.item i optionNames)
            )
            let quadSelection = players[turn - 1].SelectQuadOption quadOption
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
          if (x = 13) then (status.DiscardTile arg3 turn)
            else (status.DiscardTile arg1[nextMove] turn)
        printfn "Player %d has discarded: %s\n" turn discadedTileName
        status.SetTile turn
        handleReaction turn discardedTile 0 false
      | _ -> failwith "Fatal error"
    
    let printPlayer i = if (players[i - 1].IsUser ()) then ("User") else ("Bot")
    printfn "Dealer - East  : %s"   (printPlayer 1)
    printfn "Second - South : %s"   (printPlayer 2)
    printfn "Third  - West  : %s\n" (printPlayer 3)
    status.Init ()
    mainLoop false false 1
    // status.Display 0 (Array.toList (Array.map (fun (x: Player) -> x.IsReady ()) players)) // for debugging
    ()
  
  /// Debug mode
  member __.RunDebug () =   // Testing correctness of point calculation algorithm
    let testPointCalc argIn bonusTiles redAndNorthBonus =
      let (_, arg1, arg2, arg3, _, _, _, arg7, _, _) = argIn
      let (isLimit, points, handList) = hands.GetPoints argIn bonusTiles redAndNorthBonus
      status.DisplayWin arg1 arg2 arg3 arg7 bonusTiles
      if (isLimit)
        then (printfn "Limit hands, %d points" points) else (printfn "Normal hands, %d points" points)
      List.map (fun (p, n) -> printfn "[%2d points] %s" p n) handList |> ignore; printfn ""
          
    testPointCalc
      (3, [ 96 ], [ (20, 23); (21, 23); (22, 23); (23, 23) ], 97, true, false, false, 0, false, false)
      ([ 98; 104; 105; 106; 107 ], [ 99; 100; 101; 102; 103 ]) (0, 0)
    testPointCalc
      (2, [ 3; 7; 11; 43; 47; 79; 83; 87; 91; 95; 99; 103; 107 ], [ ], 97, true, true, false, 0, false, false)
      ([ 98; 104; 105; 106; 107 ], [ 99; 100; 101; 102; 103 ]) (0, 0)
    testPointCalc
      (1, [ 8; 9; 10; 14; 18; 22; 26; 30; 34; 38; 41; 42; 43 ], [ ], 11, true, true, false, 0, false, false)
      ([ 98; 104; 105; 106; 107 ], [ 99; 100; 101; 102; 103 ]) (0, 0)
    testPointCalc
      (3, [ 0; 1; 2; 4; 5; 6; 8; 9; 40; 41; 42; 76; 77 ], [ ], 78, true, true, false, 0, false, false)
      ([ 98; 104; 105; 106; 107 ], [ 99; 100; 101; 102; 103 ]) (0, 0)
    testPointCalc
      (1, [ 100; 101; 104; 105 ], [ (2, 21); (20, 21); (24, 21) ], 102, false, false, false, 2, false, true)
      ([ 40; 41; 42; 43; 106 ], [ 92; 93; 94; 95; 107 ]) (0, 0)
    testPointCalc
      (1, [ 44; 68; 72; 76 ], [ (20, 21); (22, 21); (24, 21) ], 45, true, false, true, 1, false, true)
      ([ 104; 105; 106; 107 ], [ 92; 93; 94; 95 ]) (0, 0)
    testPointCalc
      (2, [ 13; 14; 17; 18; 21; 22; 24; 27; 29; 30; 33; 34; 37 ], [ ], 38, true, false, false, 1, true, true)
      ([ 8; 9; 10; 11 ], [ 15; 16; 19; 20 ]) (1, 4)
    testPointCalc
      (3, [ 76 ], [ (20, 23); (22, 23); (25, 23); (26, 53) ], 79, false, false, true, 0, true, true)
      ([ 92; 93; 94; 95 ], [ 86; 87; 104; 105 ]) (0, 0)
    ()