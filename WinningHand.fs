namespace TermProject

type SpecialWinningHand (name, tileCondition, points) =
  member __.Name = name
  member __.IsSatisfied handTileList finalTile = tileCondition handTileList finalTile
  member __.Points = points

type SituationWinningHand (name, condition, pointsOpen, pointsClosed) =
  member __.Name = name
  member __.IsSatisfied player isDrawn isFirstRound isQuad isReady inOneTurn isLastTile hasCalledOpen =
    ((pointsOpen > 0) || (not hasCalledOpen))
      && (condition player isDrawn isFirstRound isQuad isReady inOneTurn isLastTile)
  member __.Points hasCalledOpen = if (hasCalledOpen) then (pointsOpen) else (pointsClosed)

type WinningHand (name, tileCondition, pointsOpen, pointsClosed, incompatible) =
  member __.Name = name
  member __.IsSatisfied player tileBuild meldList finalTile isDrawn hasCalledOpen =
    ((pointsOpen > 0) || (not hasCalledOpen))
      && (tileCondition player tileBuild meldList finalTile isDrawn)
  member __.Points hasCalledOpen = if (hasCalledOpen) then (pointsOpen) else (pointsClosed)
  member __.Incompatible = incompatible

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
  let terminalsAndHonors = List.append terminalTiles honorTiles
  let checkAllTilesIn template tileList =
    List.forall (fun x -> List.contains (x / 4) template) tileList
  let isQuad x =
    List.contains x [ 21; 22; 23; 32; 33; 34; 36; 37; 38; 42; 43; 44; 46; 47; 48 ]
  let rec tryBuild tileList doTriplet doSequence numPair numOrphan =
    let minN = (2 * numPair + numOrphan)
    match List.length tileList with
    | x when (not (x % 3 = minN % 3)) || (x < minN) -> [ ]
    | x when x = minN ->
      let currTile = tileList.Head / 4
      let try1 =   // Case 1: This tile is pair
        if (
          (numPair > 0) && (currTile = tileList.Tail.Head / 4)
            && ((minN = 2) || (not (currTile = tileList.Tail.Tail.Head / 4)))
        ) then (
          if (minN = 2) then ( [ [ (false, currTile, 2) ] ] ) else (
            let temp = tryBuild tileList.Tail.Tail false false (numPair - 1) numOrphan
            if (List.length temp = 0) then ( [ ] ) else (
              List.map (fun x -> List.append [ (false, currTile, 2) ] x) temp
            )
          )
        ) else ( [ ] )
      let try2 =   // Case 2: This tile is orphan
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
      let try1 =   // Case 1: This tile is triplet
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
      let try2 =   // Case 2: This tile is sequence
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
      let try3 =   // Case 3: This tile is pair
        if (
          (numPair > 0) && (currTile = tileList.Tail.Head / 4)
            && (not (currTile = tileList.Tail.Tail.Head / 4))
        ) then (
          let temp = tryBuild tileList.Tail.Tail doTriplet doSequence (numPair - 1) numOrphan
          if (List.length temp = 0) then ( [ ] ) else (
            List.map (fun x -> List.append [ (false, currTile, 2) ] x) temp
          )
        ) else ( [ ] )
      let try4 =   // Case 4: This tile is orphan
        if (numOrphan > 0) then (
          let temp = tryBuild tileList.Tail doTriplet doSequence numPair (numOrphan - 1)
          if (List.length temp = 0) then ( [ ] ) else (
            List.map (fun x -> List.append [ (false, currTile, 1) ] x) temp
          )
        ) else ( [ ] )
      List.append (List.append try1 try2) (List.append try3 try4)
  // winning hands - limit hands
  let situationLimitHands = [
    SituationWinningHand (
      "Blessing of Heaven",
      (
        fun player isDrawn isFirstRound isQuad isReady inOneTurn isLastTile ->
          (player = 1) && (isDrawn) && (isFirstRound)
      ),
      0, 1
    );
    SituationWinningHand (
      "Blessing of Earth",
      (
        fun player isDrawn isFirstRound isQuad isReady inOneTurn isLastTile ->
          ((player = 2) || (player = 3)) && (isDrawn) && (isFirstRound)
      ),
      0, 1
    )
  ]
  let thirteenOrphanHands = [
    SpecialWinningHand (
      "Thirteen Orphans | 13-tile Wait",
      (
        fun handTileList finalTile ->
          (List.contains (finalTile / 4) terminalsAndHonors) && (
            List.fold
              (fun x y -> if (List.exists (fun z -> (z / 4) = y) handTileList) then (x) else (false))
              true terminalsAndHonors
          )
      ),
      2
    );
    SpecialWinningHand (
      "Thirteen Orphans",
      (
        fun handTileList finalTile ->
          let allTileList = List.sort (List.append handTileList [ finalTile ])
          (List.exists (fun z -> (List.fold (countTile z) 0 allTileList) = 2) terminalsAndHonors) && (
            List.fold
              (fun x y -> if (List.exists (fun z -> z / 4 = y) allTileList) then (x) else (false))
              true terminalsAndHonors
          )
      ),
      1
    )
  ]
  let nineGatesHands = [
    SpecialWinningHand (
      "Pure Nine Gates",
      (
        fun handTileList finalTile ->
          (
            (List.contains (finalTile / 4) circleTiles)
              && (List.forall (fun x -> List.fold (countTile x) 0 handTileList = 3) [ 2; 10 ])
              && (List.forall (fun x -> List.fold (countTile x) 0 handTileList = 1) [ 3..9 ])
          ) || (
            (List.contains (finalTile / 4) bambooTiles)
              && (List.forall (fun x -> List.fold (countTile x) 0 handTileList = 3) [ 11; 19 ])
              && (List.forall (fun x -> List.fold (countTile x) 0 handTileList = 1) [ 12..18 ])
          )
      ),
      2
    );
    SpecialWinningHand (
      "Nine Gates",
      (
        fun handTileList finalTile ->
          let allTileList = List.sort (List.append handTileList [ finalTile ])
          (
            (checkAllTilesIn circleTiles allTileList) && (
              List.fold (
                fun x y ->
                  let surplus =
                    List.fold (countTile y) 0 allTileList - (if ((y = 2) || (y = 10)) then (3) else (1))
                  match (x, surplus) with
                  | (x, y) when (x < 0) || (y < 0) || (y > 1) -> -1
                  | (x, y) -> if (x + y > 1) then (-1) else (x + y)
              ) 0 [ 2..10 ] = 1
            )
          ) || (
            (checkAllTilesIn bambooTiles allTileList) && (
              List.fold (
                fun x y ->
                  let surplus =
                    List.fold (countTile y) 0 allTileList - (if ((y = 11) || (y = 19)) then (3) else (1))
                  match (x, surplus) with
                  | (x, y) when (x < 0) || (y < 0) || (y > 1) -> -1
                  | (x, y) -> if (x + y > 1) then (-1) else (x + y)
              ) 0 [ 11..19 ] = 1
            )
          )
      ),
      1
    )
  ]
  let canSevenPairLimitHands = [
    WinningHand (
      "All Honors",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.forall (fun (t, _) -> List.contains t honorTiles) meldList) && (
            List.forall (
              fun (b, t, n) -> (not b) && (List.contains t honorTiles) && ((n = 2) || (n = 3))
            ) tileBuild
          )
      ),
      1, 1, [ ]
    )
  ]
  let standardLimitHands = [
    WinningHand (
      "Big Four Winds",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.forall (fun (x, _) -> List.contains x windTiles) meldList) && (
            List.forall
              (fun x -> List.contains (false, x, 3) tileBuild)
              (List.filter (fun x -> not (List.contains x (List.map (fun (x, _) -> x) meldList))) windTiles)
          )
      ),
      2, 2, [ ]
    );
    WinningHand (
      "Four Concealed Triplets | Single Wait",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          ((List.length meldList = 0) || (List.forall (fun (_, y) -> y = 20 + player) meldList))
            && (List.forall (fun (b, t, n) -> (not b) && ((n = 2) || (n = 3))) tileBuild)
            && (List.exists (fun (b, t, n) -> (not b) && (t = finalTile / 4) && (n = 2)) tileBuild)
      ),
      0, 2, [ "Four Concealed Triplets" ]
    );
    WinningHand (
      "All Green",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          let greenTiles = [ 12; 13; 14; 16; 18; 25 ]
          (List.forall (fun (x, _) -> List.contains x greenTiles) meldList) && (
            List.forall (
              fun (b, t, n) ->
                ((b) && (t = 12) && (n = 3))
                  || ((not b) && (List.contains t greenTiles) && ((n = 2) || (n = 3)))
            ) tileBuild
          )
      ),
      1, 1, [ ]
    );
    WinningHand (
      "Big Three Dragons",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          let req =
            List.filter (fun x -> not (List.contains x (List.map (fun (x, _) -> x) meldList))) dragonTiles
          (List.length req = 0) || (List.forall (fun x -> List.contains (false, x, 3) tileBuild) req)
      ),
      1, 1, [ ]
    );
    WinningHand (
      "Small Four Winds",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          let req1 =
            List.filter (fun x -> not (List.contains x (List.map (fun (x, _) -> x) meldList))) windTiles
          (List.exists (fun x -> List.contains (false, x, 2) tileBuild) req1) && (
            let req2 = List.filter (fun x -> not (List.contains (false, x, 2) tileBuild)) req1
            List.forall (fun x -> List.contains (false, x, 3) tileBuild) req2
          )
      ),
      1, 1, [ ]
    );
    WinningHand (
      "All Terminals",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.forall (fun (t, _) -> List.contains t terminalTiles) meldList) && (
            List.forall
              (fun (b, t, n) -> (not b) && (List.contains t terminalTiles) && ((n = 2) || (n = 3)))
              tileBuild
          )
      ),
      1, 1, [ ]
    );
    WinningHand (
      "Four Concealed Triplets",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (isDrawn) && (List.forall (fun (b, t, n) -> (not b) && ((n = 2) || (n = 3))) tileBuild) && (
            (List.length meldList = 0) || (List.forall (fun (_, y) -> y = 20 + player) meldList)
          ) && (List.exists (fun (b, t, n) -> (not b) && (t = finalTile / 4) && (n = 3)) tileBuild)
      ),
      0, 1, [ ]
    );
    WinningHand (
      "Four Quads",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.length tileBuild = 1) && (List.forall (fun (b, t, n) -> (not b) && (n = 2)) tileBuild)
            && (List.length meldList = 4) && (List.forall (fun (_, y) -> isQuad y) meldList)
      ),
      1, 1, [ ]
    )
  ]
  // winning hands - normal hands
  let situationNormalHands = [
    SituationWinningHand (
      "Double Ready",
      (
        fun player isDrawn isFirstRound isQuad isReady inOneTurn isLastTile ->
          isReady = 2
      ),
      0, 2
    );
    SituationWinningHand (
      "Ready",
      (
        fun player isDrawn isFirstRound isQuad isReady inOneTurn isLastTile ->
          isReady = 1
      ),
      0, 1
    );
    SituationWinningHand (
      "One-shot",
      (
        fun player isDrawn isFirstRound isQuad isReady inOneTurn isLastTile ->
          (isReady > 0) && (inOneTurn)
      ),
      0, 1
    );
    SituationWinningHand (
      "Self-pick",
      (
        fun player isDrawn isFirstRound isQuad isReady inOneTurn isLastTile ->
          isDrawn
      ),
      0, 1
    );
    SituationWinningHand (
      "After a Quad",
      (
        fun player isDrawn isFirstRound isQuad isReady inOneTurn isLastTile ->
          (isDrawn) && (isQuad)
      ),
      1, 1
    );
    SituationWinningHand (
      "Under the Sea",
      (
        fun player isDrawn isFirstRound isQuad isReady inOneTurn isLastTile ->
          (isDrawn) && (not isQuad) && (isLastTile)
      ),
      1, 1
    );
    SituationWinningHand (
      "Under the River",
      (
        fun player isDrawn isFirstRound isQuad isReady inOneTurn isLastTile ->
          (not isDrawn) && (isLastTile)
      ),
      1, 1
    );
    SituationWinningHand (
      "Robbing a Quad",
      (
        fun player isDrawn isFirstRound isQuad isReady inOneTurn isLastTile ->
          (not isDrawn) && (isQuad)
      ),
      1, 1
    )
  ]
  let sevenPairHand = [
    WinningHand (
      "Seven Pairs",
      (
        fun player tileBuild meldList finalTile isDrawn -> true
      ),
      2, 2, [ ]
    )
  ]
  let canSevenPairNormalHands = [
    WinningHand (
      "Perfect Flush",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (
            (List.forall (fun (t, _) -> List.contains t circleTiles) meldList) && (
              List.forall (
                fun (b, t, n) ->
                  ((b) && (List.contains t [ 2..8 ]) && (n = 3))
                    || ((not b) && (List.contains t circleTiles) && ((n = 2) || (n = 3)))
              ) tileBuild
            )
          ) || (
            (List.forall (fun (t, _) -> List.contains t bambooTiles) meldList) && (
              List.forall (
                fun (b, t, n) ->
                  ((b) && (List.contains t [ 2..8 ]) && (n = 3))
                    || ((not b) && (List.contains t bambooTiles) && ((n = 2) || (n = 3)))
              ) tileBuild
            )
          )
      ),
      5, 6, [ "Common Flush" ]
    );
    WinningHand (
      "Common Flush",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (
            let temp1 = List.append circleTiles honorTiles
            (List.forall (fun (t, _) -> List.contains t temp1) meldList) && (
              List.forall (
                fun (b, t, n) ->
                  ((b) && (List.contains t [ 2..8 ]) && (n = 3))
                    || ((not b) && (List.contains t temp1) && ((n = 2) || (n = 3)))
              ) tileBuild
            )
          ) || (
            let temp2 = List.append bambooTiles honorTiles
            (List.forall (fun (t, _) -> List.contains t temp2) meldList) && (
              List.forall (
                fun (b, t, n) ->
                  ((b) && (List.contains t [ 11..17 ]) && (n = 3))
                    || ((not b) && (List.contains t temp2) && ((n = 2) || (n = 3)))
              ) tileBuild
            )
          )
      ),
      2, 3, [ ]
    );
    WinningHand (
      "All Terminals and Honors",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.forall (fun (x, _) -> List.contains x terminalsAndHonors) meldList) && (
            List.forall
              (fun (b, t, n) -> (not b) && (List.contains t terminalsAndHonors) && ((n = 2) || (n = 3)))
              tileBuild
          )
      ),
      2, 2, [ "Perfect Ends"; "Common Ends" ]
    );
    WinningHand (
      "All Simples",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          let simpleTiles = List.append [ 3..9 ] [ 12..18 ]
          (List.forall (fun (x, _) -> List.contains x simpleTiles) meldList) && (
            List.forall (
              fun (b, t, n) ->
                ((b) && (List.contains t (List.append [ 3..7 ] [ 12..16 ])) && (n = 3))
                  || ((not b) && (List.contains t simpleTiles) && ((n = 2) || (n = 3)))
            ) tileBuild
          )
      ),
      1, 1, [ ]
    )
  ]
  let standardNormalHands = [
    WinningHand (
      "Perfect Ends",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.forall (fun (x, _) -> List.contains x terminalTiles) meldList) && (
            List.forall (fun (b, t, n) ->
              ((b) && (List.contains t [ 2; 8; 11; 17 ]) && (n = 3))
                || ((not b) && (List.contains t terminalTiles) && ((n = 2) || (n = 3)))
            ) tileBuild
          )
      ),
      2, 3, [ "Common Ends" ]
    );
    WinningHand (
      "Double Identical Sequences",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.length meldList = 0) && (
            let sequences =
              List.map (fun (_, t, _) -> t) (List.filter (fun (b, _, n) -> (b) && (n = 3)) tileBuild)
            (List.length sequences = 4)
              && (List.item 1 sequences = List.item 2 sequences)
              && (List.item 3 sequences = List.item 4 sequences)
          )
      ),
      0, 3, [ "Single Identical Sequences" ]
    );
    WinningHand (
      "Full Straight",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.length meldList < 2) && (
            (List.forall (fun x -> List.contains (true, x, 3) tileBuild) [ 2 ; 5 ; 8 ])
              || (List.forall (fun x -> List.contains (true, x, 3) tileBuild) [ 11 ; 14 ; 17 ])
          )
      ),
      1, 2, [ ]
    );
    // Three Mixed Sequences is impossible in 3-person mahjong
    WinningHand (
      "Three Mixed Triplets",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          let req1 =
            List.filter
              (fun x -> not (List.contains x (List.map (fun (x, _) -> x) meldList))) [ 0; 2; 11 ]
          let req2 = 
            List.filter
              (fun x -> not (List.contains x (List.map (fun (x, _) -> x) meldList))) [ 1; 10; 19 ]
          (List.forall (fun x -> List.contains (false, x, 3) tileBuild) req1)
            || (List.forall (fun x -> List.contains (false, x, 3) tileBuild) req2)
      ),
      2, 2, [ ]
    );
    WinningHand (
      "Common Ends",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.forall (fun (x, _) -> List.contains x terminalsAndHonors) meldList) && (
            List.forall (
              fun (b, t, n) ->
                ((b) && (List.contains t [ 2; 8; 11; 17 ]) && (n = 3))
                  || ((not b) && (List.contains t terminalsAndHonors) && ((n = 2) || (n = 3)))
            ) tileBuild
          )
      ),
      1, 2, [ ]
    );
    WinningHand (
      "Little Three Dragons",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          let req1 =
            List.filter (fun x -> not (List.contains x (List.map (fun (x, _) -> x) meldList))) dragonTiles
          (List.exists (fun x -> List.contains (false, x, 2) tileBuild) req1) && (
            let req2 =
              List.filter (fun x -> not (List.contains (false, x, 2) tileBuild)) req1
            List.forall (fun x -> List.contains (false, x, 3) tileBuild) req2
          )
      ),
      2, 2, [ ]
    );
    WinningHand (
      "All Triplets",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          List.forall (fun (b, t, n) -> (not b) && ((n = 2) || (n = 3))) tileBuild
      ),
      2, 2, [ ]
    );
    WinningHand (
      "Three Concealed Triplets",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          let meldCount =
            List.fold (fun x (_, z) -> if (z = 20 + player) then (x + 1) else (x)) 0 meldList
          (
            (isDrawn) && (
              List.fold (
                fun x (b, t, n) ->
                  if ((not b) && (n = 3)) then (x + 1) else (x)
              ) 0 tileBuild + meldCount = 3
            )
          ) || (
            (not isDrawn) && (
              List.fold (
                fun x (b, t, n) ->
                  if ((not b) && (not (t = finalTile / 4)) && (n = 3)) then (x + 1) else (x)
              ) 0 tileBuild + meldCount = 3
            )
          )
      ),
      2, 2, [ ]
    );
    WinningHand (
      "Three Quads",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.length meldList > 2)
            && (List.fold (fun x (_, y) -> if (isQuad y) then (x + 1) else (x)) 0 meldList = 3)
      ),
      2, 2, [ ]
    );
    WinningHand (
      "Basic Hand",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.length meldList = 0) && (
            List.forall (
              fun (b, t, n) ->
              ((b) && (n = 3)) || (
                (not b) && (not (t = 20)) && (not (t = 19 + player))
                  && (not (List.contains t dragonTiles)) && (n  = 2)
              )
            ) tileBuild
          ) && (
            List.exists (
              fun (b, t, n) ->
                (b) && (
                  ((t = finalTile / 4) && (List.contains t (List.append [ 2..7 ] [ 11..16 ])))
                    || ((t + 2 = finalTile / 4) && (List.contains t (List.append [ 3..8 ] [ 12..17 ])))
                )
            ) tileBuild
          )
      ),
      1, 1, [ ]
    );
    WinningHand (
      "Round Wind | East",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.exists (fun (t, _) -> t = 20) meldList)
            || (List.exists (fun (b, t, n) -> (not b) && (t = 20) && (n = 3)) tileBuild)
      ),
      1, 1, [ ]
    );
    WinningHand (
      "Seat Wind | East",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (player = 1) && (
            (List.exists (fun (t, _) -> t = 20) meldList)
              || (List.exists (fun (b, t, n) -> (not b) && (t = 20) && (n = 3)) tileBuild)
          )
      ),
      1, 1, [ ]
    );
    WinningHand (
      "Seat Wind | South",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (player = 2) && (
            (List.exists (fun (t, _) -> t = 21) meldList)
              || (List.exists (fun (b, t, n) -> (not b) && (t = 21) && (n = 3)) tileBuild)
          )
      ),
      1, 1, [ ]
    );
    WinningHand (
      "Seat Wind | West",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (player = 3) && (
            (List.exists (fun (t, _) -> t = 22) meldList)
              || (List.exists (fun (b, t, n) -> (not b) && (t = 22) && (n = 3)) tileBuild)
          )
      ),
      1, 1, [ ]
    );
    WinningHand (
      "Dragon Tile | White",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.exists (fun (t, _) -> t = 24) meldList)
            || (List.exists (fun (b, t, n) -> (not b) && (t = 24) && (n = 3)) tileBuild)
      ),
      1, 1, [ ]
    );
    WinningHand (
      "Dragon Tile | Green",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.exists (fun (t, _) -> t = 25) meldList)
            || (List.exists (fun (b, t, n) -> (not b) && (t = 25) && (n = 3)) tileBuild)
      ),
      1, 1, [ ]
    );
    WinningHand (
      "Dragon Tile | Red",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.exists (fun (t, _) -> t = 26) meldList)
            || (List.exists (fun (b, t, n) -> (not b) && (t = 26) && (n = 3)) tileBuild)
      ),
      1, 1, [ ]
    );
    WinningHand (
      "Single Identical Sequences",
      (
        fun player tileBuild meldList finalTile isDrawn ->
          (List.length meldList < 3) && (
            let sequences =
              List.map (fun (_, t, _) -> t) (List.filter (fun (b, _, n) -> (b) && (n = 3)) tileBuild)
            (List.length sequences > 1) && (
              let (temp, _) =
                List.fold (
                  fun x y ->
                    let (found, prev) = x
                    if ((found) || (prev = y)) then (true, prev) else (false, y)
                ) (false, -1) sequences
              temp
            )
          )
      ),
      1, 1, [ ]
    )
  ]
  let typeAHands = List.append thirteenOrphanHands nineGatesHands
  let typeBHands = List.append situationLimitHands situationNormalHands
  let typeCHands =
    List.append canSevenPairLimitHands (List.append sevenPairHand canSevenPairNormalHands)
  let typeDHands =
    List.append
      (List.append canSevenPairLimitHands  standardLimitHands)
      (List.append canSevenPairNormalHands standardNormalHands)

  // helper functions
  let countApplicable target arg13 arg2 =
    (List.fold (fun a b -> if (b / 4 = target) then (a + 1) else (a)) 0 arg13) + (
      List.fold (
        fun a (b, c) ->
          if (b = target) then (a + (if (isQuad c) then (4) else (3))) else (a)
      ) 0 arg2
    )
  let hasCalledOpen arg0 arg2 = not (List.forall (fun (_, y) -> y = 20 + arg0) arg2)
  let isWinning argIn =
    let (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) = argIn
    let arg10 = hasCalledOpen arg0 arg2
    let tileBuild1 = tryBuild (List.sort (List.append arg1 [ arg3 ])) false false 7 0
    let tileBuild2 = tryBuild (List.sort (List.append arg1 [ arg3 ])) true  true  1 0
    let buildSuccess1 = List.length tileBuild1 > 0
    let buildSuccess2 = List.length tileBuild2 > 0
    (
      (List.length arg2 = 0) && (
        List.exists (fun (x: SpecialWinningHand) -> x.IsSatisfied arg1 arg3) typeAHands
      )
    ) || (
      (buildSuccess1 || buildSuccess2) && (
        List.exists
          (fun (x: SituationWinningHand) -> x.IsSatisfied arg0 arg4 arg5 arg6 arg7 arg8 arg9 arg10)
          typeBHands
      )
    ) || (
      (List.length tileBuild1 > 0) && (
        List.exists (
          fun x ->
            List.exists (fun (y: WinningHand) -> y.IsSatisfied arg0 x arg2 arg3 arg4 arg10) typeCHands
        ) tileBuild1
      )
    ) || (
      (List.length tileBuild2 > 0) && (
        List.exists (
          fun x ->
            List.exists (fun (y: WinningHand) -> y.IsSatisfied arg0 x arg2 arg3 arg4 arg10) typeDHands
        ) tileBuild2
      )
    )
    
  // argIn guide:
  // player handTileList meldList finalTile isDrawn isFirstRound isQuad isReady inOneTurn isLastTile
  // [0]    [1]          [2]      [3]       [4]     [5]          [6]    [7]     [8]       [9]

  /// Win check for specific hand
  member __.IsThisHand handName argIn =
    let (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) = argIn
    let arg10 = hasCalledOpen arg0 arg2
    let tileBuild1 = tryBuild (List.sort (List.append arg1 [ arg3 ])) false false 7 0
    let tileBuild2 = tryBuild (List.sort (List.append arg1 [ arg3 ])) true  true  1 0
    let isA = List.exists (fun (x: SpecialWinningHand)   -> x.Name = handName) typeAHands
    let isB = List.exists (fun (x: SituationWinningHand) -> x.Name = handName) typeBHands
    let isC = List.exists (fun (x: WinningHand)          -> x.Name = handName) typeCHands
    let isD = List.exists (fun (x: WinningHand)          -> x.Name = handName) typeDHands
    match (isA, isB, isC, isD) with
    | (true,  _,     _,     _)     ->
      let thisHand =
        List.find (fun (x: SpecialWinningHand) -> x.Name = handName) typeAHands
      (List.length arg2 = 0) && (thisHand.IsSatisfied arg1 arg3)
    | (false, true,  _,     _)     ->
      let thisHand =
        List.find (fun (x: SituationWinningHand) -> x.Name = handName) typeBHands
      (
        (List.length tileBuild1 > 0) || (List.length tileBuild2 > 0) || (
          (List.length arg2 = 0) && (
            List.exists (fun (x: SpecialWinningHand) -> x.IsSatisfied arg1 arg3) typeAHands
          )
        )
      ) && (thisHand.IsSatisfied arg0 arg4 arg5 arg6 arg7 arg8 arg9 arg10)
    | (false, false, true,  true)  -> 
      let thisHand =
        List.find (fun (x: WinningHand) -> x.Name = handName) typeCHands
      (
        (List.length tileBuild1 > 0)
          && (List.exists (fun x -> thisHand.IsSatisfied arg0 x arg2 arg3 arg4 arg10) tileBuild1)
      ) || (
        (List.length tileBuild2 > 0)
          && (List.exists (fun x -> thisHand.IsSatisfied arg0 x arg2 arg3 arg4 arg10) tileBuild2)
      )
    | (false, false, true,  false) -> 
      let thisHand =
        List.find (fun (x: WinningHand) -> x.Name = handName) typeCHands
      let hasCalledOpen =
        List.exists (fun (_, y) -> List.contains y [ 32; 33; 34; 36; 37; 38; 42; 43; 44; 46; 47; 48 ]) arg2
      (List.length tileBuild2 > 0)
        && (List.exists (fun x -> thisHand.IsSatisfied arg0 x arg2 arg3 arg4 arg10) tileBuild1)
    | (false, false, false, true)  -> 
      let thisHand =
        List.find (fun (x: WinningHand) -> x.Name = handName) typeDHands
      let hasCalledOpen =
        List.exists (fun (_, y) -> List.contains y [ 32; 33; 34; 36; 37; 38; 42; 43; 44; 46; 47; 48 ]) arg2
      (List.length tileBuild1 > 0)
        && (List.exists (fun x -> thisHand.IsSatisfied arg0 x arg2 arg3 arg4 arg10) tileBuild2)
    | (false, false, false, false) -> failwith "No such winning hand"

  /// Win check
  member __.IsWinning argIn = isWinning argIn

  /// Check if declaring ready is possible
  member __.CanDeclareReady argIn =
    let (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) = argIn
    let test13Tiles newList =
      List.filter (
        fun x ->
          let temp = List.sort (List.append newList [ x ])
          (
            (List.length arg2 = 0)
              && (List.exists (fun (x: SpecialWinningHand) -> x.IsSatisfied newList arg3) typeAHands)
          ) || (List.length (tryBuild temp false false 7 0) > 0)
            || (List.length (tryBuild temp true  true  1 0) > 0)
      ) [ for i in 0..26 -> (4 * i + 3) ]
    let candidates =
      List.map (
        fun x ->
          let newList =
            List.append
              (List.map (fun (_, z) -> z) (List.filter (fun (y, _) -> not (y = x)) (List.indexed arg1)))
              [ arg3 ]
          (x, test13Tiles newList)
      ) [ 0..(List.length arg1 - 1) ]
      |> List.filter (fun (x, l) -> List.length l > 0)
    let testFinal = test13Tiles arg1
    if (List.length testFinal > 0) then (List.append candidates [ (13, testFinal) ]) else (candidates)
  
  /// Check if one more tile can directly let you win - called for when not ready
  member __.IsOneAway argIn =
    let (arg0, arg1, arg2, _, _, arg5, arg6, arg7, arg8, arg9) = argIn
    List.filter (
      fun x -> isWinning (arg0, arg1, arg2, (4 * x + 3), false, arg5, arg6, arg7, arg8, arg9)
    ) [ 0..26 ]
  
  /// Calculate points
  member __.GetPoints argIn bonusTiles redAndNorthBonus =
    let (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) = argIn
    let arg10 = hasCalledOpen arg0 arg2
    let (defaultBonus, hiddenBonus) = bonusTiles
    let tileBuild1 = tryBuild (List.sort (List.append arg1 [ arg3 ])) false false 7 0
    let tileBuild2 = tryBuild (List.sort (List.append arg1 [ arg3 ])) true  true  1 0
    let buildSuccess1 = List.length tileBuild1 > 0
    let buildSuccess2 = List.length tileBuild2 > 0
    // helper function for testing out each hand combination
    let testThisHand subject handList =
      let (temp1, temp2, _) =
        List.fold (
          fun x (y: WinningHand) ->
            let (currPoint, currList, currExcl) = x
            if (
              (not (List.contains (y.Name) currExcl)) && (y.IsSatisfied arg0 subject arg2 arg3 arg4 arg10)
            ) then (
              (
                currPoint + (y.Points arg10),
                List.append currList [ (y.Points arg10, y.Name) ],
                List.append currExcl y.Incompatible
              )
            ) else (x)
        ) (0, [ ], [ ]) handList
      (temp1, temp2)
    let tryBlessing =   // Try Blessing series
      if (
        (situationLimitHands.Head).IsSatisfied arg0 arg4 arg5 arg6 arg7 arg8 arg9 arg10
      ) then (1) else (
        if ((situationLimitHands.Tail.Head).IsSatisfied arg0 arg4 arg5 arg6 arg7 arg8 arg9 arg10)
          then (2) else (0)
      )
    let insertBlessing argIn =
      let (points, handList) = argIn
      match tryBlessing with
      | 0 -> argIn
      | x when (x = 1) || (x = 2) ->
        let target =
          if (x = 1) then (situationLimitHands.Head) else (situationLimitHands.Tail.Head)
        (
          (points + (target.Points arg10)),
          (List.append handList [ (target.Points arg10, target.Name) ])
        )
      | _ -> failwith "Fatal error"
    let try1 =   // Try Thirteen Orphans
      let (target1, target2) = (thirteenOrphanHands.Head, thirteenOrphanHands.Tail.Head)
      if (target1.IsSatisfied arg1 arg3) then (
        [ insertBlessing (target1.Points, [ (target1.Points, target1.Name) ]) ]
      ) else (
        if (target2.IsSatisfied arg1 arg3) then (
          [ insertBlessing (target2.Points, [ (target2.Points, target2.Name) ]) ]
        ) else ( [ ] )
      )
    let try2 =   // Try Nine Gates
      let (target1, target2) = (nineGatesHands.Head, nineGatesHands.Tail.Head)
      if (target1.IsSatisfied arg1 arg3) then (
        [ insertBlessing (target1.Points, [ (target1.Points, target1.Name) ]) ]
      ) else (
        if (target2.IsSatisfied arg1 arg3) then (
          [ insertBlessing (target2.Points, [ (target2.Points, target2.Name) ]) ]
        ) else ( [ ] )
      )
    let try3 =   // Try limit hands with seven pair combinations
      if (buildSuccess1) then (
        List.filter (fun (x, y) -> x > 0) (
          List.map (fun x ->
            let (temp1, temp2) = testThisHand x canSevenPairLimitHands
            if (temp1 = 0) then (0, [ ]) else (insertBlessing (temp1, temp2))
          ) tileBuild1
        )
      ) else ( [ ] )
    let try4 =   // Try limit hands with standard combinations
      if (buildSuccess2) then (
        List.map (fun x ->
          let (temp1, temp2) =
            testThisHand x (List.append canSevenPairLimitHands standardLimitHands)
          if (temp1 = 0) then (0, [ ]) else (insertBlessing (temp1, temp2))
        ) tileBuild2
      ) else ( [ ] )
    let try5 =   // Try Blessing series on their own
      if (buildSuccess1 || buildSuccess2)
        then ( [ insertBlessing (0, [ ]) ] ) else ( [ ] )
    let allLimitTries = List.append (List.append (List.append try1 try2) (List.append try3 try4)) try5
    if (List.exists (fun (p, _) -> p > 0) allLimitTries) then (
      let (finalPoint, finalList) =
        List.fold (
          fun x y -> 
            let (points1, _) = x
            let (points2, _) = y
            if (points2 > points1) then (y) else (x)
        ) (0, [ ]) allLimitTries
      (true, finalPoint, finalList)
    ) else (   // Try normal hands
      let trySituationNormalHands =
        List.fold (
          fun x (y: SituationWinningHand) ->
            let (points, handList) = x
            (points + (y.Points arg10), List.append handList [ (y.Points arg10, y.Name) ])
        ) (0, [ ]) (
          List.filter
            (fun x -> x.IsSatisfied arg0 arg4 arg5 arg6 arg7 arg8 arg9 arg10) situationNormalHands
        )
      let insertSituationNormalHands argIn =
        let (addPoint,  addList)  = trySituationNormalHands
        if (addPoint = 0) then (argIn) else (
          let (currPoint, currList) = argIn
          (currPoint + addPoint, List.append currList addList)
        )
      let try6 =
        if (buildSuccess1) then (
          List.filter (fun (x, y) -> x > 0) (
            List.map (fun x ->
              let (temp1, temp2) =
                testThisHand x (List.append sevenPairHand canSevenPairNormalHands)
              if (temp1 = 0) then (0, [ ]) else (insertSituationNormalHands (temp1, temp2))
            ) tileBuild1
          )
        ) else ( [ ] )
      let try7 =
        if (buildSuccess2) then (
          List.filter (fun (x, y) -> x > 0) (
            List.map (fun x ->
              let (temp1, temp2) =
                testThisHand x (List.append canSevenPairNormalHands standardNormalHands)
              if (temp1 = 0) then (0, [ ]) else (insertSituationNormalHands (temp1, temp2))
            ) tileBuild2
          )
        ) else ( [ ] )
      let try8 =
        if (buildSuccess1 || buildSuccess2)
          then ( [ insertSituationNormalHands (0, [ ]) ] ) else ( [ ] )
      let allNormalTries = List.append (List.append try6 try7) try8
      if (List.exists (fun (p, _) -> p > 0) allNormalTries) then (
        let (finalPoint, finalList) =
          List.fold (
            fun x y ->
              let (points1, _) = x
              let (points2, _) = y
              if (points2 > points1) then (y) else (x)
          ) (0, [ ]) allNormalTries
        let arg13 = List.append arg1 [ arg3 ]
        let (redBonusPoint, northBonusPoint) = redAndNorthBonus
        let bonusList1 =
          if (redBonusPoint > 0)
            then (List.append finalList [ (redBonusPoint, "Red 5 Tiles") ]) else (finalList)
        let bonusList2 =
          if (northBonusPoint > 0)
            then (List.append bonusList1 [ (northBonusPoint, "North Tiles Put Aside") ]) else (bonusList1)
        let defaultBonusPoint =
          List.fold (
            fun x y ->
              match (y / 4) with
              |  1 -> x + countApplicable  0 arg13 arg2
              | 10 -> x + countApplicable  2 arg13 arg2
              | 19 -> x + countApplicable 11 arg13 arg2
              | 23 -> x + countApplicable 20 arg13 arg2
              | 26 -> x + countApplicable 24 arg13 arg2
              |  z -> x + countApplicable (z + 1) arg13 arg2
          ) 0 defaultBonus
        let bonusList3 =
          if (defaultBonusPoint > 0)
            then (List.append bonusList2 [ (defaultBonusPoint, "Bonus Tiles") ]) else (bonusList2)
        let hiddenBonusPoint =
          if (arg7 > 0) then (
            List.fold (
              fun x y ->
                match (y / 4) with
                |  1 -> x + countApplicable  0 arg13 arg2
                | 10 -> x + countApplicable  2 arg13 arg2
                | 19 -> x + countApplicable 11 arg13 arg2
                | 23 -> x + countApplicable 20 arg13 arg2
                | 26 -> x + countApplicable 24 arg13 arg2
                |  z -> x + countApplicable (z + 1) arg13 arg2
            ) 0 hiddenBonus
          ) else (0)
        let bonusList4 =
          if (hiddenBonusPoint > 0)
            then (List.append bonusList3 [ (hiddenBonusPoint, "Hidden Bonus Tiles") ]) else (bonusList3)
        (
          false,
          finalPoint + redBonusPoint + northBonusPoint + defaultBonusPoint + hiddenBonusPoint,
          bonusList4
        )
      ) else (false, 0, [ ])
    )
