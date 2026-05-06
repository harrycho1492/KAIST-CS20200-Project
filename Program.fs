/// KAIST CS.20200 Term Project
module TermProject.GameEntry

let rec gameInit () =
  printfn "Select an option:"
  printfn "[1] Play as dealer / East"
  printfn "[2] Play as second / South"
  printfn "[3] Play as third  / West"
  printfn "[4] Exit"
  System.Console.Write "> "
  match System.Console.ReadLine () with
  | "1" | "01" -> Mahjong(1).Run(); gameInit ()
  | "2" | "02" -> Mahjong(2).Run(); gameInit ()
  | "3" | "03" -> Mahjong(3).Run(); gameInit ()
  | "4" | "04" -> 0
  | _ -> printfn "\n[*] Invalid option.\n"; gameInit ()

[<EntryPoint>]
let main _args =
  printfn "Project Title: CLI 3-person Japanese Mahjong\n"
  gameInit ()
