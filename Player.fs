namespace TermProject

/// Mahjong player
type Player (playerIsUser) =
  let rand = System.Random ()

  member __.IsUser () = playerIsUser
  member __.NextMove (tileList: int array) =
    if (playerIsUser) then (
      let rec getSelection () =
        printfn "Select the tile to discard:"
        System.Console.Write "> "
        match System.Console.ReadLine () with
        | "01" | "1" -> 0
        | "02" | "2" -> 1
        | "03" | "3" -> 2
        | "04" | "4" -> 3
        | "05" | "5" -> 4
        | "06" | "6" -> 5
        | "07" | "7" -> 6
        | "08" | "8" -> 7
        | "09" | "9" -> 8
        | "10"       -> 9
        | "11"       -> 10
        | "12"       -> 11
        | "13"       -> 12
        | "14"       -> 13
        | _ -> printfn "\n[*] Invalid option.\n"; getSelection ()
      tileList[getSelection ()]
    ) else (
      tileList[rand.Next (0, 14)]
    )