namespace TermProject

/// Mahjong player
type Player (playerIsUser, hands: WinningHands) =
  let rand = System.Random ()
  // Player states
  let mutable hasDeclaredReady = 0
  let mutable inOneTurn        = false
  let mutable hasTempPenalty   = 0
  let mutable target13Orphans  = false         // Only used for bots
  let mutable quadOptionChoice = (false, -1)   // Only used for bots

  // helper functions
  let terminalsAndHonors = [ 0; 1; 2; 10; 11; 19; 20; 21; 22; 23; 24; 25; 26 ]

  let countTile cond list =
    List.fold (fun x y -> if (cond y) then (x + 1) else (x)) 0 list
  
  let removeOneTile target indexedList =
    List.map (fun (x, y) -> y) (List.filter (fun (x, _) -> not (x = target)) indexedList)

  let getAppeal arg0 arg1 arg2 arg5 arg6 arg7 arg8 arg9 bonusTiles disclosedTiles =
    List.fold (
      fun a b ->
        let r = 4 - (List.item b disclosedTiles)
        let newHand = (arg0, arg1, arg2, b, true, arg5, arg6, arg7, arg8, arg9)
        if (hands.IsWinning newHand) then (
          let (out0, out1, _) = hands.GetPoints newHand bonusTiles (0, 0)
          if (out0) then (a + r * 130 * out1) else (a + r * 10 * out1)
        ) elif (List.length (hands.CanDeclareReady newHand) > 0) then (a + r * 5) else (a)
    ) 0 [ 0..26 ]

  /// Bot's tile discard strategy for conventional hands
  let nextDiscard statusInfo =
    let (handInfo, bonusTiles, disclosedTiles) = statusInfo
    let (arg0, arg1, arg2, arg3, _, arg5, arg6, arg7, arg8, arg9) = handInfo
    let arg13 =
      if (arg3 < 0) then (List.indexed arg1) else (List.append (List.indexed arg1) [ (13, arg3) ])
    let appeal =
      List.map (
        fun (i, t) ->
          let discHand = removeOneTile i arg13
          let expected =
            getAppeal arg0 discHand arg2 arg5 arg6 arg7 arg8 arg9 bonusTiles disclosedTiles
          if (expected > 0) then (i, - expected) else (
            let tx = t / 4
            let cnt1 = countTile (fun y -> y / 4 = tx) discHand
            if (cnt1 > 1) then (i, 2 * (cnt1 - 1)) else (
              (i, (countTile (
                fun y ->
                  ((List.contains tx (List.append [ 2..9 ] [ 11..18 ])) && (y / 4 = tx + 1))
                    || ((List.contains tx (List.append [ 3..10 ] [ 12..19 ])) && (y / 4 = tx - 1))
              ) discHand))
            )
          )
      ) arg13
    List.fold (
      fun (outV, outList) (i, v) ->
        if   (v < outV) then (v, [i])
        elif (v = outV) then (outV, List.append outList [i]) else (outV, outList)
    ) (400, []) appeal
  
  let checkStrategyAbort statusInfo =
    let (handInfo, bonusTiles, disclosedTiles) = statusInfo
    let (_, arg1, _, arg3, _, _, _, _, _, _) = handInfo
    List.exists (
      fun x ->
        (List.item x disclosedTiles = 4) && (countTile (fun y -> y / 4 = x) (List.append arg1 [ arg3 ]) = 0)
    ) terminalsAndHonors
  
  /// Bot's tile discard strategy for Thirteen Orphans limit hand
  let thirteenOrphanStrategy statusInfo =
    if (checkStrategyAbort statusInfo) then (
      target13Orphans <- false; nextDiscard statusInfo
    ) else (
      let (handInfo, bonusTiles, disclosedTiles) = statusInfo
      let (_, arg1, _, arg3, _, _, _, _, _, _) = handInfo
      let arg13 =
        if (arg3 < 0) then (List.indexed arg1) else (List.append (List.indexed arg1) [ (13, arg3) ])
      let appeal =
        List.map (
          fun (i, t) ->
            if (not (List.contains (t / 4) terminalsAndHonors)) then (i, -5)
            else (i, 1 - countTile (fun y -> y / 4 = t / 4) (List.append arg1 [ arg3 ]))
        ) arg13
      let (out0, out1) =
        List.fold (
          fun (outV, outList) (i, v) ->
          if   (v < outV) then (v, [i])
          elif (v = outV) then (outV, List.append outList [i]) else (outV, outList)
        ) (1, []) appeal
      if (List.exists (fun x -> if (x = 13) then (arg3 / 4 = 23) else ((List.item x arg1) / 4 = 23)) out1)
        then (out0, [17]) else (out0, out1)
    )

  /// Bot's strategy for determining the limit hand
  let exploreQuadOpt quadOption statusInfo =
    let temp =
      List.tryFind (
        fun (b, t, _) -> (b) || ((not b) && (List.contains t [ 0; 1; 20; 21; 22; 23; 24; 25; 26 ]))
      ) quadOption
    match temp with
    | Some (b, t, _) -> quadOptionChoice <- (b, t); true
    | None           ->
      let (handInfo, bonusTiles, disclosedTiles) = statusInfo
      let (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) = handInfo
      let (expected1, _) = nextDiscard statusInfo
      let expected2 =
        List.map (
          fun (b, t, _) ->
            if (b) then (failwith "Fatal error") else (
              let narg1 = List.filter (fun x -> t / 4 <> x) (List.sort (List.append arg1 [ arg3 ]))
              let narg2 = List.append arg2 [ (t, 20 + arg0) ]
              (b, t, getAppeal arg0 narg1 narg2 arg5 arg6 arg7 arg8 arg9 bonusTiles disclosedTiles)
            )
        ) quadOption
      match List.tryFind (fun (b, t, e) -> e > (- expected1)) expected2 with
      | Some (b, t, _) -> quadOptionChoice <- (b, t); true
      | None           -> false

  member __.IsUser () = playerIsUser

  member __.IsReady () = hasDeclaredReady
  member __.DeclareReady isFirstRound =
    if (isFirstRound) then (hasDeclaredReady <- 2; inOneTurn <- true;)
      else (hasDeclaredReady <- 1; inOneTurn <- true;)

  member __.InOneTurn    () = inOneTurn
  member __.EndOneTurn   () = inOneTurn <- false;

  member __.HasTempPenalty () = hasTempPenalty > 0
  member __.GivePenalty () =
    if (hasDeclaredReady > 0) then (hasTempPenalty <- 2;) else (hasTempPenalty <- 1;)
  member __.RemovePenalty () =
    if (hasTempPenalty < 2) then (hasTempPenalty <- 0;)

  member __.NextMove valid isWinning isReady canReady quadOption has4zTile canAbort statusInfo =
    if (
      (isReady) && (not isWinning) && (List.length quadOption = 0) && (not has4zTile) && (canAbort < 9)
    ) then (
      if (playerIsUser) then (printfn "Automatically discarding drawn tile...\n"; 13) else (13)
    ) else (
      if (playerIsUser) then (
        let rec getSelection () =
          let checkAndReturn n =
            if ((not isReady) && (n > valid))
              then (printfn "\n[*] Invalid option.\n"; getSelection ()) else (printfn ""; n)
          printfn "Select the tile to discard / Select action:"
          System.Console.Write "> "
          match System.Console.ReadLine () with
          | "01" | "1" -> checkAndReturn 0
          | "02" | "2" -> checkAndReturn 1
          | "03" | "3" -> checkAndReturn 2
          | "04" | "4" -> checkAndReturn 3
          | "05" | "5" -> checkAndReturn 4
          | "06" | "6" -> checkAndReturn 5
          | "07" | "7" -> checkAndReturn 6
          | "08" | "8" -> checkAndReturn 7
          | "09" | "9" -> checkAndReturn 8
          | "10"       -> checkAndReturn 9
          | "11"       -> checkAndReturn 10
          | "12"       -> checkAndReturn 11
          | "13"       -> checkAndReturn 12
          | "14"       -> printfn ""; 13
          | "15"       ->
            if (isWinning) then (printfn ""; 14)
              else (printfn "\n[*] Invalid option.\n"; getSelection ())
          | "16"       ->
            if (canReady) then (printfn ""; 15)
              else (printfn "\n[*] Invalid option.\n"; getSelection ())
          | "17"       ->
            if (List.length quadOption > 0) then (printfn ""; 16)
              else (printfn "\n[*] Invalid option.\n"; getSelection ())
          | "18"       ->
            if (has4zTile) then (printfn ""; 17)
              else (printfn "\n[*] Invalid option.\n"; getSelection ())
          | "19"       ->
            if (canAbort > 8) then (printfn ""; 18)
              else (printfn "\n[*] Invalid option.\n"; getSelection ())
          | _ -> printfn "\n[*] Invalid option.\n"; getSelection ()
        getSelection ()
      ) else (
        match (isWinning, canReady) with
        | (true,  _)     -> 14
        | (false, true)  -> 15
        | (false, false) ->
          if   (canAbort = 9) then (18)
          elif ((canAbort < 9) && (not target13Orphans) && (exploreQuadOpt quadOption statusInfo))
            then (16)
          elif ((canAbort < 9) && (not target13Orphans) && (has4zTile)) then (17) else (
            if (canAbort > 9) then (target13Orphans <- true)
            let (out0, out1) =
              if (target13Orphans) then (thirteenOrphanStrategy statusInfo) else (nextDiscard statusInfo)
            List.item (rand.Next (0, List.length out1)) out1   // printfn "%d %A" out0 out1
          )
      )
    )
  
  member __.SelectReadyOption readyOpt disclosedTiles =
    if (playerIsUser) then (
      let readyIdx = List.map fst readyOpt
      let rec getSelection () =
        let checkAndReturn n =
          if (List.contains n readyIdx) then (printfn ""; n)
            else (printfn "\n[*] Invalid option.\n"; getSelection ())
        printfn "Select the tile to discard:"
        System.Console.Write "> "
        match System.Console.ReadLine () with
        | "01" | "1" -> checkAndReturn 0
        | "02" | "2" -> checkAndReturn 1
        | "03" | "3" -> checkAndReturn 2
        | "04" | "4" -> checkAndReturn 3
        | "05" | "5" -> checkAndReturn 4
        | "06" | "6" -> checkAndReturn 5
        | "07" | "7" -> checkAndReturn 6
        | "08" | "8" -> checkAndReturn 7
        | "09" | "9" -> checkAndReturn 8
        | "10"       -> checkAndReturn 9
        | "11"       -> checkAndReturn 10
        | "12"       -> checkAndReturn 11
        | "13"       -> checkAndReturn 12
        | "14"       -> checkAndReturn 13
        | _ -> printfn "\n[*] Invalid option.\n"; getSelection ()
      getSelection ()
    ) else (
      let (_, bestChoices) =
        List.fold (
          fun (n, ol) (x, l) ->
            let y = List.fold (fun cnt z -> cnt + (4 - (List.item x disclosedTiles))) 0 l
            if (y > n) then (y, [x]) elif (y = n) then (n, List.append ol [x]) else (n, ol)
        ) (0, []) readyOpt
      List.item (rand.Next (0, List.length bestChoices)) bestChoices
    )
    
  member __.SelectQuadOption quadOption =
    if (playerIsUser) then (
      let rec getSelection () =
        let checkAndReturn n =
          if (n > (List.length quadOption - 1))
            then (printfn "\n[*] Invalid option.\n"; getSelection ()) else (printfn ""; n)
        printfn "Select the tile to meld:"
        System.Console.Write "> "
        match System.Console.ReadLine () with
        | "01" | "1" -> checkAndReturn 0
        | "02" | "2" -> checkAndReturn 1
        | "03" | "3" -> checkAndReturn 2
        | "04" | "4" -> checkAndReturn 3
        | _ -> printfn "\n[*] Invalid option.\n"; getSelection ()
      getSelection ()
    ) else (
      if (quadOptionChoice = (false, -1)) then (failwith "Fatal error")
      let out = List.findIndex (fun (b, t, _) -> (b, t) = quadOptionChoice) quadOption
      quadOptionChoice <- (false, -1); out
    )
  
  member __.DoWin () =
    if (playerIsUser) then (
      let rec getSelection () =
        printfn "Enter [1] to win by discard, or enter [2] to skip:"
        System.Console.Write "> "
        match System.Console.ReadLine () with
        | "01" | "1" -> true
        | "02" | "2" -> false
        | _ -> printfn "\n[*] Invalid option.\n"; getSelection ()
      getSelection ()
    ) else (true)
  
  member __.DoQuad actionPlayer statusInfo =
    if (playerIsUser) then (
      let rec getSelection () =
        printfn "Enter [1] to make an open quad, or enter [2] to skip:"
        System.Console.Write "> "
        match System.Console.ReadLine () with
        | "01" | "1" -> true
        | "02" | "2" -> false
        | _ -> printfn "\n[*] Invalid option.\n"; getSelection ()
      getSelection ()
    ) else (
      if ((target13Orphans) && (not (checkStrategyAbort statusInfo))) then (false) else (
        if (target13Orphans) then (target13Orphans <- false)
        let (handInfo, bonusTiles, disclosedTiles) = statusInfo
        let (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) = handInfo
        let narg1 = List.filter (fun x -> not (x / 4 = arg3 / 4)) arg1
        let narg2 = List.append arg2 [ (arg3 / 4, 27 + actionPlayer * 3 + arg0) ]
        let expected1 =
          getAppeal arg0  arg1  arg2 arg5 arg6 arg7 arg8 arg9 bonusTiles disclosedTiles
        let expected2 =
          getAppeal arg0 narg1 narg2 arg5 arg6 arg7 arg8 arg9 bonusTiles disclosedTiles
        (expected2 > expected1)
      )
    )
  
  member __.DoTri actionPlayer statusInfo =
    if (playerIsUser) then (
      let rec getSelection () =
        printfn "Enter [1] to make an called triplet, or enter [2] to skip:"
        System.Console.Write "> "
        match System.Console.ReadLine () with
        | "01" | "1" -> true
        | "02" | "2" -> false
        | _ -> printfn "\n[*] Invalid option.\n"; getSelection ()
      getSelection ()
    ) else (
      if ((target13Orphans) && (not (checkStrategyAbort statusInfo))) then (false) else (
        if (target13Orphans) then (target13Orphans <- false)
        let (handInfo, bonusTiles, disclosedTiles) = statusInfo
        let (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) = handInfo
        let narg1 =
          arg1
          |> List.indexed |> removeOneTile (List.findIndex (fun x -> x / 4 = arg3 / 4) arg1)
          |> List.indexed |> removeOneTile (List.findIndex (fun x -> x / 4 = arg3 / 4) arg1)
        let narg2 = List.append arg2 [ (arg3 / 4, 47 + actionPlayer * 3 + arg0) ]
        let expected1 =
          getAppeal arg0  arg1  arg2 arg5 arg6 arg7 arg8 arg9 bonusTiles disclosedTiles
        let (expected2, _) =
          nextDiscard
            ((arg0, narg1, narg2, -1, false, arg5, arg6, arg7, arg8, arg9), bonusTiles, disclosedTiles)
        (- expected2 > expected1)
      )
    )
  
  member __.DoDiscard newHand statusInfo =
    let valid = Array.length newHand - 1
    if (playerIsUser) then (
      let rec getSelection () =
        let checkAndReturn n =
          if (n > valid)
            then (printfn "\n[*] Invalid option.\n"; getSelection ()) else (printfn ""; n)
        printfn "Select the tile to discard:"
        System.Console.Write "> "
        match System.Console.ReadLine () with
        | "01" | "1" -> checkAndReturn 0
        | "02" | "2" -> checkAndReturn 1
        | "03" | "3" -> checkAndReturn 2
        | "04" | "4" -> checkAndReturn 3
        | "05" | "5" -> checkAndReturn 4
        | "06" | "6" -> checkAndReturn 5
        | "07" | "7" -> checkAndReturn 6
        | "08" | "8" -> checkAndReturn 7
        | "09" | "9" -> checkAndReturn 8
        | "10"       -> checkAndReturn 9
        | "11"       -> checkAndReturn 10
        | "12"       -> checkAndReturn 11
        | "13"       -> checkAndReturn 12
        | _ -> printfn "\n[*] Invalid option.\n"; getSelection ()
      getSelection ()
    ) else (
      let (out0, out1) = nextDiscard statusInfo
      List.item (rand.Next (0, List.length out1)) out1   // printfn "%d %A" out0 out1
    )