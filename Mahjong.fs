namespace TermProject

type Mahjong (userPos) =
  let status = GameStatus ()
  let players =
    match userPos with
    | 1 -> [| true; false; false |]
    | 2 -> [| false; true; false |]
    | 3 -> [| false; false; true |]
    | _ -> failwith "Fatal error"
    // TBA - Add further information in players list

  member __.Run () =
    status.Init ()
    let printPlayer i = if (players[i]) then ("User") else ("Bot")
    printfn "\nDealer - East  : %s"   (printPlayer 0)
    printfn   "Second - South : %s"   (printPlayer 1)
    printfn   "Third  - West  : %s\n" (printPlayer 2)
    status.Display 0
    printfn "The actual game isn't here as of now,"
    printfn "but content will be added in due time...\n"
    // TBA
