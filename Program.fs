/// KAIST CS.20200 Term Project
module TermProject.GameEntry

let rec gameInit () =
  printfn "Select an option:"
  printfn "[1] Play as dealer / East\n[2] Play as second / South"
  printfn "[3] Play as third  / West\n[4] Exit"
  System.Console.Write "> "
  match System.Console.ReadLine () with
  | "1" | "01" -> printfn ""; Mahjong(1).Run(); gameInit ()
  | "2" | "02" -> printfn ""; Mahjong(2).Run(); gameInit ()
  | "3" | "03" -> printfn ""; Mahjong(3).Run(); gameInit ()
  | "4" | "04" -> 0
  | "x"        ->                                        // Debugging keyword 1
    printfn ""; for i in 1..60 do Mahjong(0).Run()
    gameInit ()
  | "y"        -> printfn ""; Mahjong(0).RunDebug(); 0   // Debugging keyword 2
  | _ -> printfn "\n[*] Invalid option.\n"; gameInit ()

[<EntryPoint>]
let main _args =
  printfn "Welcome to Japanese Mahjong!\n"; gameInit ()
