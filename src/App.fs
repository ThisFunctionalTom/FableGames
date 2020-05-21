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

    let pageHtml page =
        match page with
        | About -> Info.View.root
        | Jamb -> Counter.View.root model.Counter (JambMsg >> dispatch)
        | Home -> Home.View.view

    Html.div [
        Navbar.View.root model.CurrentPage
        Bulma.section [
            Bulma.container [
                Bulma.columns [
                    Bulma.column [
                        pageHtml model.CurrentPage ] ] ] ] ]

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
