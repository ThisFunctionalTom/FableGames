module App.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Types
open App.State
open Global

importAll "../sass/main.sass"

open Feliz
open Feliz.Bulma

let root model dispatch =
    let navbarMenu page =
        match page with
        | Jamb -> Jamb.Game.navbarMenu model.Jamb (JambMsg >> dispatch)
        | Home -> Bulma.navbarMenu [ ]

    let pageHtml page =
        match page with
        | Jamb -> Jamb.Game.view model.Jamb (JambMsg >> dispatch)
        | Home -> Home.View.view

    Html.div [
        Navbar.View.root (navbarMenu model.CurrentPage)
        Bulma.section [ Bulma.container [ pageHtml model.CurrentPage ] ] ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run
