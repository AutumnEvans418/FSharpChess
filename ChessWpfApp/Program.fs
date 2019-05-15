// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System.Windows
open System.Windows.Controls

let showWindow =
    let window = new Window()
    window.Show()
    window
let createGrid (window:Window) =
    let grid = new Grid()
    window.Content = (grid :> obj) |> ignore
    grid

let updateGridCell grid =
    
let showGridWindow grid gridView =
    ()
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
