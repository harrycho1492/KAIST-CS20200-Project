namespace TermProject

type Mahjong (userPos) =
  let status = GameStatus ()
  let hands  = WinningHands ()
  let players = [| for i in 1..3 -> if (i = userPos) then (Player (true)) else (Player (false)) |]
    // TBA - Add further information in players list
  
  /// Debug mode
  member __.RunDebug () =
    // WinningHand debugging
    let testWinningHand handName argIn =
      printfn "Check %s:" handName
      if (hands.IsThisHand handName argIn) then (printfn "OK\n") else (printfn "Error\n")
    testWinningHand "Thirteen Orphans | 13-tile Wait"
      ([| 0; 4; 8; 40; 44; 76; 80; 84; 88; 92; 96; 100; 104 |], [ ], 1, true, false)
    testWinningHand "Pure Nine Gates"
      ([| 8; 9; 10; 12; 16; 20; 24; 28; 32; 36; 40; 41; 42 |], [ ], 11, true, false)
    testWinningHand "Big Four Winds"
      ([| 80; 81; 82; 84; 85; 86; 88; 89; 90; 92; 93; 94; 96 |], [ ], 97, true, false)
    testWinningHand "Four Concealed Triplets | Single Wait"
      ([| 80; 81; 82; 84; 96; 97; 98; 100; 101; 102; 104; 105; 106 |], [ ], 87, true, false)
    testWinningHand "Thirteen Orphans"
      ([| 4; 8; 40; 44; 76; 80; 84; 88; 92; 96; 100; 104; 105 |], [ ], 0, true, false)
    testWinningHand "Nine Gates"
      ([| 44; 45; 48; 52; 56; 60; 64; 68; 72; 76; 77; 78; 79 |], [ ], 47, true, false)
    testWinningHand "All Green"
      ([| 48; 49; 52; 53; 56; 57; 64; 65; 66; 100 |], [ (18, 34) ], 101, false, true)
    testWinningHand "Big Three Dragons"
      ([| 0; 1; 8; 13; 18; 104; 105 |], [ (24, 44); (25, 47) ], 106, false, true)
    testWinningHand "All Terminals"
      ([| 0; 1; 2; 4; 5; 6; 8; 9; 10; 40; 41; 42; 44 |], [ ], 45, true, false)
    testWinningHand "All Honors"
      ([| 80; 81; 84; 85; 88; 89; 92; 93; 96; 97; 100; 101; 104 |], [ ], 105, true, false)
    testWinningHand "Four Concealed Triplets"
      ([| 80; 81; 82; 84; 85; 86; 88; 89; 90; 92; 93; 96; 97 |], [ ], 94, true, false)
    testWinningHand "Four Quads"
      ([| 96 |], [ (20, 21); (21, 34); (22, 44); (23, 47) ], 97, false, true)
    testWinningHand "Seven Pairs"
      ([| 12; 13; 16; 17; 20; 21; 24; 28; 29; 32; 33; 36; 37 |], [ ], 25, false, false)
    testWinningHand "All Terminals and Honors"
      ([| 0; 1; 2; 4; 5; 6; 96; 97; 100; 101; 104; 105; 106 |], [ ], 98, false, false)
    testWinningHand "All Triplets"
      ([| 12; 13; 14; 16; 17; 18; 24; 32; 33; 34; 36; 37; 38 |], [ ], 25, false, false)

    ()

  member __.Run () =
    let rec mainLoop turn =
      let handleReaction () =
        // TBA - reaction from other players
        // TBA - check if there is no reaction
        // check for no winning hand abort & call next player
        if (status.CanDrawTile ()) then (
          mainLoop ((turn % 3) + 1)
        ) else (
          printfn "No player was able to complete a winning hand.\n"
          ()
        )

      // draw tile
      let drawnTile = status.DrawTile turn
      printfn "Player %d has drawn: %s\n" turn drawnTile
      let handInfo = status.ConstructHand turn
      let (arg1, _, arg3, _, _) = handInfo
      let isWinning = hands.IsWinning handInfo
      let makeQuad  = status.TryMakeMeld turn
      if (players[turn - 1].IsUser ()) then (
        status.Display turn
        if (isWinning) then (
          printfn "Winning possible: Enter [15] to claim victory"
        )
        match Array.length makeQuad with
        | 0 -> printfn ""
        | 1 -> printfn "Making closed quad possible: Enter [17] to perform action\n"
        | 2 -> printfn "Making closed quad possible (2 options): Enter [17] or [18] to perform action\n"
        | 3 -> printfn "Making closed quad possible (3 options): Enter [17], [18] or [19] to perform action\n"
        | _ -> failwith "Fatal error"
      )
      let nextMove =
        players[turn - 1].NextMove (Array.length arg1 - 1) isWinning (Array.length makeQuad)
        // TBA - should provide (status.ConstructInfo turn) as argument for bot information
      match nextMove with
      | 14 ->
        printfn "Player %d has won!\n" turn
        // TBA - calculate score
        ()
      // TBA - claiming ready (15)
      | 16 | 17 | 18 ->
        let meldedTile = status.MakeMeld makeQuad[nextMove - 16] turn
        printfn "Player %d has melded 4 tiles into a closed quad: %s\n" turn meldedTile
        status.SetTile turn
        handleReaction ()
      | x when ((x > -1) && (x < 14)) ->
        let discardedTile =
          if (x = 13) then (
            status.DiscardTile arg3 turn
          ) else (
            status.DiscardTile arg1[nextMove] turn
          )
        printfn "Player %d has discarded: %s\n" turn discardedTile
        status.SetTile turn
        handleReaction ()
      | _ -> failwith "Fatal error"
    
    let printPlayer i = if (players[i - 1].IsUser ()) then ("User") else ("Bot")
    printfn "Dealer - East  : %s"   (printPlayer 1)
    printfn "Second - South : %s"   (printPlayer 2)
    printfn "Third  - West  : %s\n" (printPlayer 3)

    status.Init ()
    status.Display 0   // for debugging

    mainLoop 1
    status.Display 0   // for debugging

    printfn "There's only drawing and discarding tiles as of now,"
    printfn "but content will be added in due time...\n"
    // TBA
    ()
