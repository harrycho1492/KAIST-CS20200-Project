namespace TermProject

type Mahjong (userPos) =
  let players =
    match userPos with
    | 1 -> [| true; false; false |]
    | 2 -> [| false; true; false |]
    | 3 -> [| false; false; true |]
    | _ -> failwith "Fatal error"
    // TBA - Add further information in players list
  // TBA - Game status indicator

  member __.Run () =
    let printPlayer i = if (players[i]) then ("User") else ("Bot")
    printfn "Dealer - East  : %s" (printPlayer 0)
    printfn "Second - South : %s" (printPlayer 1)
    printfn "Third  - West  : %s" (printPlayer 2)
    printfn "The game isn't here as of now,"
    printfn "but content will be added in due time..."
    // TBA
