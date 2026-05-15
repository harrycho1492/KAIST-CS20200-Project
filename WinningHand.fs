namespace TermProject

type WinningHand (name, tileCondition, pointsOpen, pointsClosed, incompatible) =
  member __.Name = name
  member __.IsSatisfied handTileList meldList finalTile isDrawn hasCalledOpen =
    (
      (pointsOpen > 0) || (not hasCalledOpen)
    ) && (
      tileCondition handTileList meldList finalTile isDrawn
    )
  member __.Points hasCalledOpen =
    if (hasCalledOpen) then (pointsOpen) else (pointsClosed)
  (*
  // TBA - do this when working with scoring
  member __.IsIncompatible otherHand =
    List.contains (fun x -> x = otherHand) incompatible
  *)

type WinningHands () =
  let countTile n =
    fun x y -> if ((y / 4) = n) then (x + 1) else (x)
  let letterTiles = [ 0; 1 ]
  let circleTiles = [  2..10 ]
  let bambooTiles = [ 11..19 ]
  let windTiles   = [ 20..23 ]
  let dragonTiles = [ 24..26 ]
  let honorTiles = List.append windTiles dragonTiles
  let terminalTiles = [ 0; 1; 2; 10; 11; 19 ]
  let rec tryBuild tileList doTriplet doSequence numPair numOrphan =
    // printfn "%A" tileList
    let minN = (2 * numPair + numOrphan)
    match List.length tileList with
    | x when (not (x % 3 = minN % 3)) || (x < minN) -> [ ]
    | x when x = minN ->
      let currTile = tileList.Head / 4
      // Case 1: This tile is pair
      let try1 =
        if ((numPair > 0) && (currTile = tileList.Tail.Head / 4)) then (
          if (minN = 2) then ( [ [ (false, currTile, 2) ] ] ) else (
            let temp = tryBuild tileList.Tail.Tail false false (numPair - 1) numOrphan
            if (List.length temp = 0) then ( [ ] ) else (
              List.map (fun x -> List.append [ (false, currTile, 2) ] x) temp
            )
          )
        ) else ( [ ] )
      // Case 2: This tile is orphan
      let try2 =
        if (numOrphan > 0) then (
          if (minN = 1) then ( [ [ (false, currTile, 1) ] ] ) else (
            let temp = tryBuild tileList.Tail false false numPair (numOrphan - 1)
            if (List.length temp = 0) then ( [ ] ) else (
              List.map (fun x -> List.append [ (false, currTile, 1) ] x) temp
            )
          )
        ) else ( [ ] )
      List.append try1 try2
    | x ->
      let currTile = tileList.Head / 4
      let nextList = tileList.Tail
      // Case 1: This tile is triplet
      let try1 =
        if (
          doTriplet && (currTile = nextList.Head / 4) && (currTile = nextList.Tail.Head / 4)
        ) then (
          if (x = 3) then ( [ [ (false, currTile, 3) ] ] ) else (
            let temp = tryBuild nextList.Tail.Tail true doSequence numPair numOrphan
            if (List.length temp = 0) then ( [ ] ) else (
              List.map (fun x -> List.append [ (false, currTile, 3) ] x) temp
            )
          )
        ) else ( [ ] )
      // Case 2: This tile is sequence
      let try2 =
        if (
          (
            doSequence
          ) && (
            List.contains currTile (List.append [ 2..8 ] [ 11..17 ])
          ) && (
            List.exists (fun x -> x / 4 = currTile + 1) nextList
          ) && (
            List.exists (fun x -> x / 4 = currTile + 2) nextList
          )
        ) then (
          if (x = 3) then ( [ [ (true, currTile, 3) ] ] ) else (
            let rm1List = List.removeAt (List.findIndex (fun x -> x / 4 = currTile + 1) nextList) nextList
            let rm2List = List.removeAt (List.findIndex (fun x -> x / 4 = currTile + 2) rm1List) rm1List
            let temp = tryBuild rm2List doTriplet true numPair numOrphan
            if (List.length temp = 0) then ( [ ] ) else (
              List.map (fun x -> List.append [ (true, currTile, 3) ] x) temp
            )
          )
        ) else ( [ ] )
      // Case 3: This tile is pair
      let try3 =
        if ((numPair > 0) && (currTile = tileList.Tail.Head / 4)) then (
          let temp = tryBuild tileList.Tail.Tail doTriplet doSequence (numPair - 1) numOrphan
          if (List.length temp = 0) then ( [ ] ) else (
            List.map (fun x -> List.append [ (false, currTile, 2) ] x) temp
          )
        ) else ( [ ] )
      // Case 4: This tile is orphan
      let try4 =
        if (numOrphan > 0) then (
          let temp = tryBuild tileList.Tail doTriplet doSequence numPair (numOrphan - 1)
          if (List.length temp = 0) then ( [ ] ) else (
            List.map (fun x -> List.append [ (false, currTile, 1) ] x) temp
          )
        ) else ( [ ] )
      List.append (List.append try1 try2) (List.append try3 try4)
  // winning hands - limit hands
  let limitHandsList = [
    WinningHand (
      "Thirteen Orphans | 13-tile Wait",
      (
        fun handTileList meldList finalTile isDrawn ->
          (List.length meldList = 0) && (
            let terminalsAndHonors = List.append terminalTiles honorTiles
            (List.contains (finalTile / 4) terminalsAndHonors) && (
              List.fold
                (fun x y -> if (List.exists (fun z -> (z / 4) = y) handTileList) then (x) else (false))
                true terminalsAndHonors
            ) 
          )
      ),
      0, 2,
      [ "Thirteen Orphans" ]
    );
    WinningHand (
      "Pure Nine Gates",
      (
        fun handTileList meldList finalTile isDrawn ->
          (List.length meldList = 0) && (
            (
              (List.contains (finalTile / 4) circleTiles) && (
                List.fold (fun x y -> if (List.fold (countTile y) 0 handTileList = 3) then (x) else (false))
                  true [ 2; 10 ]
              ) && (
                List.fold (fun x y -> if (List.fold (countTile y) 0 handTileList = 1) then (x) else (false))
                  true [ 3..9 ]
              )
            ) || (
              (List.contains finalTile bambooTiles) && (
                List.fold (fun x y -> if (List.fold (countTile y) 0 handTileList = 3) then (x) else (false))
                  true [ 11; 19 ]
              ) && (
                List.fold (fun x y -> if (List.fold (countTile y) 0 handTileList = 1) then (x) else (false))
                  true [ 12..18 ]
              )
            )
          )
      ),
      0, 2,
      [ "Nine Gates" ]
    );
    WinningHand (
      "Big Four Winds",
      (
        fun handTileList meldList finalTile isDrawn ->
          (List.forall (fun (x, _) -> List.contains x windTiles) meldList) && (
            let req =
              List.filter (fun x -> not (List.contains x (List.map (fun (x, _) -> x) meldList))) windTiles
            (
              (List.length req = 0) && (List.length handTileList = 1) && (
                handTileList.Head / 4 = finalTile / 4
              )
            ) || (
              let temp = tryBuild (List.sort (List.append handTileList [ finalTile ])) true false 1 0
              (List.length temp > 0) && (
                List.exists (
                  fun x ->
                    List.forall (fun z -> List.exists (fun (b, t, n) -> (not b) && (t = z) && (n = 3)) x) req
                ) temp
              )
            )
          )
      ),
      2, 2,
      [ "Small Four Winds" ]
    );
    WinningHand (
      "Four Concealed Triplets | Single Wait",
      (
        fun handTileList meldList finalTile isDrawn ->
          (
            (List.length meldList = 0) || (List.forall (fun (_, y) -> List.contains y [ 21..23 ] ) meldList)
          ) && (
            let temp = tryBuild handTileList true false 0 1
            (List.length temp > 0) && (
              List.exists
                (fun x -> List.exists (fun (b, t, n) -> (not b) && (t = finalTile / 4) && (n = 1)) x) temp
            )
          )
      ),
      0, 2,
      [ "Four Concealed Triplets" ]
    );
    WinningHand (
      "Thirteen Orphans",
      (
        fun handTileList meldList finalTile isDrawn ->
          (List.length meldList = 0) && (
            let terminalsAndHonors = List.append terminalTiles honorTiles
            let allTileList = List.sort (List.append handTileList [ finalTile ])
            (List.exists (fun z -> (List.fold (countTile z) 0 allTileList) = 2) terminalsAndHonors) && (
              List.fold
                (fun x y -> if (List.exists (fun z -> z / 4 = y) allTileList) then (x) else (false))
                true terminalsAndHonors
            )
          )
      ),
      0, 1,
      [ ]
    );
    // TBA - Blessing of Heaven, Blessing of Earth
    WinningHand (
      "Nine Gates",
      (
        fun handTileList meldList finalTile isDrawn ->
          (List.length meldList = 0) && (
            let allTileList = List.sort (List.append handTileList [ finalTile ])
            (
              (
                (
                  [ 2; 10 ] |> List.fold (
                    fun x y ->
                      match (x, List.fold (countTile y) 0 allTileList) with
                      | (-1, _) -> -1
                      | ( x, 3) -> x
                      | ( 0, 4) -> 1
                      | ( 1, 4) -> -1
                      | _ -> -1
                  ) 0 = 1
                ) && (
                  [ 3..9 ]
                    |> List.fold (fun x y -> if ((List.fold (countTile y) 0 allTileList) = 1) then (x) else (false)) true
                )
              ) || (
                (
                  [ 2; 10 ]
                    |> List.fold (fun x y -> if ((List.fold (countTile y) 0 allTileList) = 3) then (x) else (false)) true
                ) && (
                  [ 3..9 ] |> List.fold (
                    fun x y ->
                      match (x, List.fold (countTile y) 0 allTileList) with
                      | (-1, _) -> -1
                      | ( x, 1) -> x
                      | ( 0, 2) -> 1
                      | ( 1, 2) -> -1
                      | _ -> -1
                  ) 0 = 1
                )
              )
            ) || (
              (
                (
                  [ 11; 19 ] |> List.fold (
                    fun x y ->
                      match (x, List.fold (countTile y) 0 allTileList) with
                      | (-1, _) -> -1
                      | ( x, 3) -> x
                      | ( 0, 4) -> 1
                      | ( 1, 4) -> -1
                      | _ -> -1
                  ) 0 = 1
                ) && (
                  [ 12..18 ]
                    |> List.fold (fun x y -> if ((List.fold (countTile y) 0 allTileList) = 1) then (x) else (false)) true
                )
              ) || (
                (
                  [ 11; 19 ]
                    |> List.fold (fun x y -> if ((List.fold (countTile y) 0 allTileList) = 3) then (x) else (false)) true
                ) && (
                  [ 12..18 ] |> List.fold (
                    fun x y ->
                      match (x, List.fold (countTile y) 0 allTileList) with
                      | (-1, _) -> -1
                      | ( x, 1) -> x
                      | ( 0, 2) -> 1
                      | ( 1, 2) -> -1
                      | _ -> -1
                  ) 0 = 1
                )
              )
            )
          )
      ),
      0, 1,
      [ ]
    );
    WinningHand (
      "All Green",
      (
        fun handTileList meldList finalTile isDrawn ->
          let greenTiles = [ 12; 13; 14; 16; 18; 25 ]
          (List.forall (fun (x, _) -> List.contains x greenTiles) meldList) && (
            let allTileList = List.sort (List.append handTileList [ finalTile ])
            let temp = tryBuild allTileList true true 1 0
            (List.length temp > 0) && (
              List.forall (fun x -> List.contains (x / 4) greenTiles) allTileList
            )
          )
      ),
      1, 1,
      [ ]
    );
    WinningHand (
      "Big Three Dragons",
      (
        fun handTileList meldList finalTile isDrawn ->
          if (List.exists (fun (x, _) -> not (List.contains x dragonTiles)) meldList) then (false) else (
            let req =
              List.filter (fun x -> not (List.contains x (List.map (fun (x, _) -> x) meldList))) dragonTiles
            let allTileList = List.sort (List.append handTileList [ finalTile ])
            let temp = tryBuild allTileList true true 1 0
            (List.length temp > 0) && (
              (List.length req = 0) || (
                List.exists
                  (
                    fun x ->
                      List.forall
                        (fun y -> List.exists (fun (b, t, n) -> (b = false) && (t = y) && (n = 3)) x) req
                  )
                  temp
              )
            )
          )
      ),
      1, 1,
      [ ]
    );
    // TBA - Small Four Winds
    WinningHand (
      "All Terminals",
      (
        fun handTileList meldList finalTile isDrawn ->
          if (List.exists (fun (x, _) -> not (List.contains x terminalTiles)) meldList) then (false) else (
            let allTileList = List.sort (List.append handTileList [ finalTile ])
            let temp = tryBuild allTileList true false 1 0
            (
              List.length temp > 0
            ) && (
              List.forall (fun x -> List.contains (x / 4) terminalTiles) allTileList
            )
          )
      ),
      1, 1,
      [ ]
    );
    WinningHand (
      "All Honors",
      (
        fun handTileList meldList finalTile isDrawn ->
          let allTileList = List.sort (List.append handTileList [ finalTile ])
          (
            (List.length meldList = 0) && (
              let temp = tryBuild allTileList false false 7 0
              (List.length temp > 0) && (List.forall (fun x -> List.contains (x / 4) honorTiles) allTileList)
            )
          ) || (
            (List.forall (fun (x, _) -> List.contains x honorTiles) meldList) && (
              let temp = tryBuild allTileList true false 1 0
              (List.length temp > 0) && (List.forall (fun x -> List.contains (x / 4) honorTiles) allTileList)
            )
          )
      ),
      1, 1,
      [ ]
    );
    WinningHand (
      "Four Concealed Triplets",
      (
        fun handTileList meldList finalTile isDrawn ->
          (isDrawn) && (
            (List.length meldList = 0) || (List.forall (fun (_, y) -> List.contains y [ 21..23 ]) meldList)
          ) && (
            let temp = tryBuild handTileList true false 2 0
            (List.length temp > 0) && (
              List.exists (
                fun x -> List.exists (fun (b, t, n) -> (not b) && (t = finalTile / 4) && (n = 2)) x
              ) temp
            )
          )
      ),
      0, 1,
      [ ]
    );
    WinningHand (
      "Four Quads",
      (
        fun handTileList meldList finalTile isDrawn ->
          (List.length meldList = 4) && (
            List.forall
              (fun (_, y) -> List.contains y [ 21; 22; 23; 32; 33; 34; 36; 37; 38; 42; 43; 44; 46; 47; 48 ])
              meldList
          ) && (List.length handTileList = 1) && (handTileList.Head / 4 = finalTile / 4)
      ),
      1, 1,
      [ ]
    )
  ]
  let normalHandsList = [
    // TBA - 6 points: Perfect Flush
    // TBA - 3 points: Common Flush, Perfect Ends, Double Identical Sequences
    WinningHand (
      "Seven Pairs",
      (
        fun handTileList meldList finalTile isDrawn ->
          (List.length meldList = 0) && (
            let temp = tryBuild (List.sort (List.append handTileList [ finalTile ])) false false 7 0
            List.length temp > 0
          )
      ),
      2, 2,
      [ ]
    );
    // TBA - 2 points: Double Ready, ..., Common Ends
    WinningHand (
      "All Terminals and Honors",
      (
        fun handTileList meldList finalTile isDrawn ->
          let allTileList = List.sort (List.append handTileList [ finalTile ])
          let terminalsAndHonors = List.append terminalTiles honorTiles
          (
            (List.length meldList = 0) && (
              let temp = tryBuild allTileList false false 7 0
              (List.length temp > 0) && (
                List.forall (fun x -> List.contains (x / 4) terminalsAndHonors) allTileList
              )
            )
          ) || (
            (List.forall (fun (x, _) -> List.contains x terminalsAndHonors) meldList) && (
              let temp = tryBuild allTileList true false 1 0
              (List.length temp > 0) && (
                List.forall (fun x -> List.contains (x / 4) terminalsAndHonors) allTileList
              )
            )
          )
      ),
      2, 2,
      [ "Common Ends" ]
    );
    // TBA - 2 points: Little Three Dragons 
    WinningHand (
      "All Triplets",
      (
        fun handTileList meldList finalTile isDrawn ->
          let temp = tryBuild (List.sort (List.append handTileList [ finalTile ])) true false 1 0
          List.length temp > 0
      ),
      2, 2,
      [ ]
    )
    // TBA - 2 points: Three Concealed Triplets, Three Quads
    // TBA - 1 points
  ]

  // For debugging - win check for specific hand
  member __.IsThisHand handName argIn =
    let (thisHand: WinningHand) =
      List.find (fun x -> x.Name = handName) (List.append limitHandsList normalHandsList)
    let (arg1, arg2, arg3, arg4, arg5) = argIn
    thisHand.IsSatisfied (Array.toList arg1) arg2 arg3 arg4 arg5

  /// Win check - input: 14 tiles
  member __.IsWinning argIn =
    let (arg1, arg2, arg3, arg4, arg5) = argIn
    (
      List.exists (fun (x: WinningHand) -> x.IsSatisfied (Array.toList arg1) arg2 arg3 arg4 arg5) limitHandsList
    ) || (
      List.exists (fun (x: WinningHand) -> x.IsSatisfied (Array.toList arg1) arg2 arg3 arg4 arg5) normalHandsList
    )
  
  /// Calculate points - must only be called when IsWinning
  member __.GetPoints argIn =
    let (arg1, arg2, arg3, arg4, arg5) = argIn
    if (
      List.exists (fun (x: WinningHand) -> x.IsSatisfied (Array.toList arg1) arg2 arg3 arg4 arg5) limitHandsList
    ) then (
      () // TBA
    ) else (
      () // TBA
    )
  
  /// Tenpai check - input: 13 tiles
  member __.IsOneAway handTileList meldList hasCalledOpen =
    () // TBA
