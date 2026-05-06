namespace TermProject

type Mahjong (userPos) =
  let status = GameStatus ()
  let players = [| for i in 1..3 -> if (i = userPos) then (true) else (false) |]
    // TBA - Add further information in players list

  member __.Run () =
    let rec mainLoop turn =
      // draw tile
      let drawnTile = status.DrawTile turn
      printfn "Player %d has drawn: %s\n" turn drawnTile
      (* if (players[turn - 1]) then (
        // TBA - call for user interaction
      ) else (
        // TBA - call for bot interaction
      ) *)
      // TBA - reaction from other players
      // TBA - check if there is no reaction
      // check for no winning hand abort & call next player
      if (status.CanDrawTile ()) then (
        mainLoop ((turn % 3) + 1)
      ) else (
        ()
      )
    
    let printPlayer i = if (players[i - 1]) then ("User") else ("Bot")
    printfn "\nDealer - East  : %s"   (printPlayer 1)
    printfn   "Second - South : %s"   (printPlayer 2)
    printfn   "Third  - West  : %s\n" (printPlayer 3)

    status.Init ()
    status.Display 0   // for debugging

    mainLoop 1
    status.Display 0   // for debugging

    printfn "The actual game isn't here as of now,"
    printfn "but content will be added in due time...\n"
    // TBA
