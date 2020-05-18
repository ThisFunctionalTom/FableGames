module App.Types

open Global

type Msg =
    | JambMsg of Counter.Types.Msg
    | HomeMsg of Home.Types.Msg

type Model =
    { CurrentPage: Page
      Counter: Counter.Types.Model
      Home: Home.Types.Model }
