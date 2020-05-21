module App.Types

open Global

type Msg =
    | JambMsg of Jamb.Game.Message
    | HomeMsg of Home.Types.Msg

type Model =
    { CurrentPage: Page
      Jamb: Jamb.Game.State
      Home: Home.Types.Model }
