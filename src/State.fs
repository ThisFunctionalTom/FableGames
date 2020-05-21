module App.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser
open Global
open Types

let pageParser: Parser<Page->Page,Page> =
    oneOf [
        map Jamb (s PageHash.Jamb)
        map Home (s PageHash.Home)
    ]

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHash model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init result =
    let (jamb, jambCmd) = Jamb.Game.init()
    let (home, homeCmd) = Home.State.init()
    let (model, cmd) =
        urlUpdate result
          { CurrentPage = Home
            Jamb = jamb
            Home = home }

    model, Cmd.batch [ cmd
                       Cmd.map JambMsg jambCmd
                       Cmd.map HomeMsg homeCmd ]

let update msg model =
    match msg with
    | JambMsg msg ->
        let (jamb, counterCmd) = Jamb.Game.update msg model.Jamb
        { model with Jamb = jamb }, Cmd.map JambMsg counterCmd
    | HomeMsg msg ->
        let (home, homeCmd) = Home.State.update msg model.Home
        { model with Home = home }, Cmd.map HomeMsg homeCmd
