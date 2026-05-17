namespace TermProject

/// Mahjong player
type Player (playerIsUser) =
  let rand = System.Random ()
  // Player states
  let mutable hasDeclaredReady = 0
  let mutable hasTempPenalty   = 0

  member __.IsUser () = playerIsUser

  member __.IsReady () = hasDeclaredReady

  member __.DeclareReady isFirstRound =
    if (isFirstRound) then (hasDeclaredReady <- 2;) else (hasDeclaredReady <- 1;)

  member __.HasTempPenalty () = hasTempPenalty > 0

  member __.GivePenalty () =
    if (hasDeclaredReady > 0) then (hasTempPenalty <- 2;) else (hasTempPenalty <- 1;)

  member __.RemovePenalty () =
    if (hasTempPenalty < 2) then (hasTempPenalty <- 0;)

  member __.NextMove valid isWinning canMakeQuad has4zTile canAbort = // TBA - should get inArg also
    if (playerIsUser) then (
      let rec getSelection () =
        let checkAndReturn n =
          if (n > valid) then (
            printfn "\n[*] Invalid option.\n"; getSelection ()
          ) else (
            printfn ""; n
          )
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
        | "17"       ->
          if (canMakeQuad) then (printfn ""; 16)
            else (printfn "\n[*] Invalid option.\n"; getSelection ())
        | "18"       ->
          if (has4zTile) then (printfn ""; 17)
            else (printfn "\n[*] Invalid option.\n"; getSelection ())
        | "19"       ->
          if (canAbort) then (printfn ""; 18)
            else (printfn "\n[*] Invalid option.\n"; getSelection ())
        | _ -> printfn "\n[*] Invalid option.\n"; getSelection ()
      getSelection ()
    ) else (
      match (isWinning, canAbort, canMakeQuad, has4zTile) with
      | (true,  _,     _,     _)    -> 14
      | (false, true,  _,     _)    -> 18
      | (false, false, true,  _)    -> 16
      | (false, false, false, true) -> 17
      | _ ->
        let temp =
          if (canMakeQuad) then (rand.Next (0, valid + 3)) else (rand.Next (0, valid + 2))
        match temp with
        | x when (x > -1) && (x < (valid + 1)) -> x
        | x when x = (valid + 1) -> 13
        | x when x = (valid + 2) -> 16
        | _ -> failwith "Fatal error"
    )
  member __.SelectQuadOption length =
    if (playerIsUser) then (
      let rec getSelection () =
        let checkAndReturn n =
          if (n > (length - 1)) then (
            printfn "\n[*] Invalid option.\n"; getSelection ()
          ) else (
            printfn ""; n
          )
        printfn "Select the tile to meld:"
        System.Console.Write "> "
        match System.Console.ReadLine () with
        | "01" | "1" -> checkAndReturn 0
        | "02" | "2" -> checkAndReturn 1
        | "03" | "3" -> checkAndReturn 2
        | _ -> printfn "\n[*] Invalid option.\n"; getSelection ()
      getSelection ()
    ) else (
      rand.Next (0, length)
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
    ) else (
      true
    )
  
  member __.DoQuad () =
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
      true
    )
  
  member __.DoTri () =
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
      true
    )
  
  member __.DoDiscard newHand =
    let valid = Array.length newHand - 1
    if (playerIsUser) then (
      let rec getSelection () =
        let checkAndReturn n =
          if (n > valid) then (
            printfn "\n[*] Invalid option.\n"; getSelection ()
          ) else (
            printfn ""; n
          )
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
      rand.Next (0, valid + 1)
    )