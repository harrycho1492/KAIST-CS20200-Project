namespace TermProject

/// Mahjong player
type Player (playerIsUser) =
  let rand = System.Random ()
  let hasCalledOpen    = false
  let hasDeclaredReady = false
  let hasTempPenalty   = false

  member __.IsUser () = playerIsUser
  member __.NextMove valid isWinning possibleQuads = // TBA - should get inArg also
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
        | "14"       -> printfn ""; 13
        | "15"       ->
          if (isWinning) then (
            printfn ""; 14
          ) else (
            printfn "\n[*] Invalid option.\n"; getSelection ()
          )
        | "17"       ->
          if (possibleQuads > 0) then (
            printfn ""; 16
          ) else (
            printfn "\n[*] Invalid option.\n"; getSelection ()
          )
        | "18"       ->
          if (possibleQuads > 1) then (
            printfn ""; 17
          ) else (
            printfn "\n[*] Invalid option.\n"; getSelection ()
          )
        | "19"       ->
          if (possibleQuads > 2) then (
            printfn ""; 18
          ) else (
            printfn "\n[*] Invalid option.\n"; getSelection ()
          )
        | _ -> printfn "\n[*] Invalid option.\n"; getSelection ()
      getSelection ()
    ) else (
      if (isWinning) then (
        14
      ) else (
        if (possibleQuads > 0) then (
          rand.Next (0, possibleQuads)
        ) else (
          let temp = rand.Next (0, valid + 2)
          if (temp > valid) then (13) else (temp)
        )
      )
    )