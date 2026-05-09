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
  let rec tryBuildTriplets tileList =
    match List.length tileList with
    | x when not (x % 3 = 0) -> [ -1 ]
    | 0 -> failwith "Fatal error"
    | 3 ->
      if (
        (tileList.Head / 4 = tileList.Tail.Head / 4) && (tileList.Head / 4 = tileList.Tail.Tail.Head / 4)
      ) then ( [ tileList.Head % 4 ] ) else ( [ -1 ] )
    | x ->
      if (
        (tileList.Head / 4 = tileList.Tail.Head / 4) && (tileList.Head / 4 = tileList.Tail.Tail.Head / 4)
      ) then (
        let nextTry = tryBuildTriplets tileList.Tail.Tail.Tail
        if (nextTry.Head = -1) then ( [ -1 ] ) else (List.append [ tileList.Head / 4 ] nextTry)
      ) else ( [ -1 ])
  let rec tryBuildTripletsAndOne tileList = 
    match List.length tileList with
    | x when not (x % 3 = 1) -> [ (-1, -1) ]
    | 1 -> [ (tileList.Head / 4, 1) ]
    | x ->
      if (tileList.Head / 4 = tileList.Tail.Head / 4) then (
        if (tileList.Head / 4 = tileList.Tail.Tail.Head / 4) then (
          // This tile is body
          let nextTry = tryBuildTripletsAndOne tileList.Tail.Tail.Tail
          if (nextTry.Head = (-1, -1)) then ( [ (-1, -1) ] ) else (
            List.append [ (tileList.Head / 4, 3) ] nextTry
          )
        ) else ( [ (-1, -1) ] )
      ) else (
        // This tile is orphan
        let nextTry = tryBuildTriplets tileList.Tail
        if (nextTry.Head = -1) then ( [ (-1, -1) ] ) else (
          List.append [ (tileList.Head / 4, 1) ] (List.map (fun x -> (x, 3)) nextTry)
        )
      )
  let rec tryBuildTripletsAndPairs tileList n =
    match List.length tileList with
    | x when ((not (x % 3 = (2 * n) % 3)) || (x < 2 * n)) -> [ (-1, -1) ]
    | x when (x = 2 * n) ->
      match n with
      | x when x < 1 -> failwith "Fatal error"
      | 1 -> if (tileList.Head / 4 = tileList.Tail.Head / 4) then ( [ (tileList.Head / 4, 2) ] ) else ( [ (-1, -1) ] )
      | x ->
        if (tileList.Head / 4 = tileList.Tail.Head / 4) then (
          let nextTry = tryBuildTripletsAndPairs tileList.Tail.Tail (n - 1)
          if (nextTry.Head = (-1, -1)) then ( [ (-1, -1) ] ) else (
            List.append [ (tileList.Head / 4, 2) ] nextTry
          )
        ) else ( [ (-1, -1) ] )
    | x ->
      if (tileList.Head / 4 = tileList.Tail.Head / 4) then (
        if (tileList.Head / 4 = tileList.Tail.Tail.Head / 4) then (
          // This tile is body
          let nextTry = tryBuildTripletsAndPairs tileList.Tail.Tail.Tail n
          if (nextTry.Head = (-1, -1)) then ( [ (-1, -1) ] ) else (
            List.append [ (tileList.Head / 4, 3) ] nextTry
          )
        ) else (
          // This tile is head
          match n with
          | x when x < 1 -> failwith "Fatal error"
          | 1 ->
            let nextTry = tryBuildTriplets tileList.Tail.Tail
            if (nextTry.Head = -1) then ( [ (-1, -1) ] ) else (
              List.append [ (tileList.Head / 4, 2) ] (List.map (fun x -> (x, 3)) nextTry)
            )
          | x -> 
            let nextTry = tryBuildTripletsAndPairs tileList.Tail.Tail (n - 1)
            if (nextTry.Head = (-1, -1)) then ( [ (-1, -1) ] ) else (
              List.append [ (tileList.Head / 4, 2) ] nextTry
            )
        )
      ) else ( [ (-1, -1) ] )
  let rec tryBuildHeads tileList =
    match List.length tileList with
    | x when not (x % 2 = 0) -> [ -1 ]
    | 0 -> failwith "Fatal error"
    | 2 ->
      if (tileList.Head / 4 = tileList.Tail.Head / 4) then ( [ tileList.Head / 4 ] ) else ( [ -1 ] )
    | x ->
      if (tileList.Head / 4 = tileList.Tail.Head / 4) then (
        if (tileList.Head / 4 = tileList.Tail.Tail.Head / 4) then ( [ -1 ] ) else (
          let nextTry = tryBuildHeads tileList.Tail.Tail
          if (nextTry.Head = -1) then ( [ -1 ] ) else (
            List.append [ tileList.Head / 4 ] nextTry
          )
        )
      ) else ( [ -1 ] )
  // winning hands - limit hands
  let limitHandsList = [
    WinningHand (
      "Thirteen Orphans | 13-tile Wait",
      (
        fun handTileList meldList finalTile isDrawn ->
          if (List.length meldList > 0) then (false) else (
            (
              List.contains (finalTile / 4) (List.append terminalTiles honorTiles)
            ) && (
              (List.append terminalTiles honorTiles)
                |> List.fold (
                  fun x y -> if (List.exists (fun z -> (z / 4) = y) handTileList) then (x) else (false)
                ) true
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
          if (List.length meldList > 0) then (false) else (
            (
              (
                List.contains (finalTile / 4) circleTiles
              ) && (
                [ 2; 10 ]
                  |> List.fold (fun x y -> if (List.fold (countTile y) 0 handTileList = 3) then (x) else (false)) true
              ) && (
                [ 3..9 ]
                  |> List.fold (fun x y -> if (List.fold (countTile y) 0 handTileList = 1) then (x) else (false)) true
              )
            ) || (
              (
                List.contains finalTile bambooTiles
              ) && (
                [ 11; 19 ]
                  |> List.fold (fun x y -> if (List.fold (countTile y) 0 handTileList = 3) then (x) else (false)) true
              ) && (
                [ 12..18 ]
                  |> List.fold (fun x y -> if (List.fold (countTile y) 0 handTileList = 1) then (x) else (false)) true
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
          if (List.exists (fun (x, _) -> not (List.contains (x / 4) windTiles)) meldList) then (false) else (
            let req =
              List.filter (fun x -> not (List.contains x (List.map (fun (x, _) -> x) meldList))) windTiles
            if (List.length req = 0) then (
              if (not (List.length handTileList = 1)) then (failwith "Fatal error") else (
                handTileList.Head = finalTile
              )
            ) else (
              let temp = tryBuildTripletsAndPairs (List.sort (List.append handTileList [ finalTile ])) 1
              if (temp.Head = (-1, -1)) then (false) else (
                List.forall (fun x -> (List.exists (fun (y, z) -> ((y = x) && (z = 3))) temp)) req
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
            (
              List.length meldList = 0
            ) || (
              not (List.exists (fun (_, y) -> not (List.contains y [ 21..23 ])) meldList)
            )
          ) && (
            let temp = tryBuildTripletsAndOne handTileList
            if (temp.Head = (-1, -1)) then (false) else (
              fst (List.find (fun (_, y) -> y = 1) temp) = finalTile / 4
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
          if (List.length meldList > 0) then (false) else (
            let allTileList = List.sort (List.append handTileList [ finalTile ])
            (
              List.exists (fun z -> (List.fold (countTile z) 0 allTileList) = 2) (List.append terminalTiles honorTiles)
            ) && (
              (List.append terminalTiles honorTiles)
                |> List.fold (fun x y -> if (List.exists (fun z -> z / 4 = y) allTileList) then (x) else (false)) true
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
          if (List.length meldList > 0) then (false) else (
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
    // TBA - All Green, Big Three Dragons, Small Four Winds
    WinningHand (
      "All Terminals",
      (
        fun handTileList meldList finalTile isDrawn ->
          if (List.exists (fun (x, _) -> not (List.contains x terminalTiles)) meldList) then (false) else (
            let temp = tryBuildTripletsAndPairs (List.sort (List.append handTileList [ finalTile ])) 1
            if (temp.Head = (-1, -1)) then (false) else (
              not (List.exists (fun (x, _) -> not (List.contains x terminalTiles)) temp)
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
          (
            (
              List.length meldList = 0
            ) && (
              let temp = tryBuildHeads (List.sort (List.append handTileList [ finalTile ]))
              (
                not (temp.Head = -1)
              ) && (
                not (List.exists (fun x -> not (List.contains x honorTiles)) temp)
              )
            )
          ) || (
            (
              not (List.exists (fun (x, _) -> not (List.contains x honorTiles)) meldList)
            ) && (
              let temp = tryBuildTripletsAndPairs (List.sort (List.append handTileList [ finalTile ])) 1
              (
                not (temp.Head = (-1, -1))
              ) && (
                not (List.exists (fun (x, _) -> not (List.contains x honorTiles)) temp)
              )
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
            (
              List.length meldList = 0
            ) || (
              not (List.exists (fun (_, y) -> not (List.contains y [ 21..23 ])) meldList)
            )
          ) && (
            let temp = tryBuildTripletsAndPairs handTileList 2
            (
              not (temp.Head = (-1, -1))
            ) && (
              List.exists (fun (x, _) -> x = finalTile / 4) (List.filter (fun (_, y) -> y = 2) temp)
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
          (
            List.length meldList = 4
          ) && (
            not (
              List.exists (
                fun (_, y) -> not (List.contains y [ 21; 22; 23; 32; 33; 34; 36; 37; 38; 42; 43; 44; 46; 47; 48 ])
              ) meldList
            )
          ) && (
            List.length handTileList = 1
          ) && (
            handTileList.Head / 4 = finalTile / 4
          )
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
          (
            List.length meldList = 0
          ) && (
            let temp = tryBuildHeads (List.sort (List.append handTileList [ finalTile ]))
            not (temp.Head = -1)
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
          (
            (
              List.length meldList = 0
            ) && (
              let temp = tryBuildHeads (List.sort (List.append handTileList [ finalTile ]))
              (
                not (temp.Head = -1)
              ) && (
                not (List.exists (fun x -> not (List.contains x (List.append terminalTiles honorTiles))) temp)
              )
            )
          ) || (
            (
              not (List.exists (fun (x, _) -> not (List.contains x (List.append terminalTiles honorTiles))) meldList)
            ) && (
              let temp = tryBuildTripletsAndPairs (List.sort (List.append handTileList [ finalTile ])) 1
              (
                not (temp.Head = (-1, -1))
              ) && (
                not (List.exists (fun (x, _) -> not (List.contains x (List.append terminalTiles honorTiles))) temp)
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
          let temp = tryBuildTripletsAndPairs (List.sort (List.append handTileList [ finalTile ])) 1
          not (temp.Head = (-1, -1))
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
