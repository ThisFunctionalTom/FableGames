module Jamb.Game

open Elmish
open Scoreboard
open DiceSet
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Switch
open Zanaptak.TypedCssClasses

type Fa = CssClasses<"../node_modules/@fortawesome/fontawesome-free/css/fontawesome.min.css", Naming.PascalCase>

[<RequireQualifiedAccess>]
module List =
    let safeTake count list =
        if List.length list < count
        then list
        else List.take count list

type GameState =
| WaitingForRoll
| WaitingForRollWithUndo of {| ScoredCell: CellId; UndoState: GameState |}
| Rolling of gameState: GameState
| RolledOnce
| WaitingForCall
| RolledOnceCalled of calledRow: RowId
| RolledTwice
| RolledTwiceCalled of calledRow: RowId
| WaitingForScore
| GameOver

type HallOfFameEntry =
    { Name: string
      Scoreboard: Scoreboard }

type HallOfFame = HallOfFameEntry list

let [<Literal>] HallOfFameEntries = 10

type State = {
    DiceSet: DiceSet
    Scoreboard: Scoreboard
    GameState: GameState
    Player: string
    HallOfFame: HallOfFame
} with
    static member Empty config hallOfFame =
        { DiceSet = DiceSet.init config
          Scoreboard = Scoreboard.Init()
          GameState = WaitingForRoll
          Player = "Kumerle"
          HallOfFame = hallOfFame }

module Test =
    open System
    let rnd = Random()

    module Player =
        let players =
            [ "Andreja"; "Megie"; "Domi"; "Luka"; "Anastasija"; "Tom";
              "Irena"; "Kumerle"; "Kazo"; "Gazda"; "Teo"; "Lucija"; "Krolo" ]

        let random() =
            List.item (rnd.Next(0, players.Length)) players

    module DiceDots =
        let random() = List.init 6 (fun _ -> rnd.Next(1, 7))

    module Scoreboard =
        let random columns =
            columns
            |> List.collect (fun colId -> rowIds |> List.map (fun rowId -> (colId, rowId), Played (DiceDots.random())))
            |> List.append (rowIds |> List.map (fun rowId -> (Call, rowId), NotPlayed))
            |> Map.ofList
            |> Scoreboard

        let filled() =
            random [ Down; Up; Free; Call ]

    module HallOfFame =
        let filled () =
            List.init HallOfFameEntries (fun _ -> Player.random())
            |> List.map (fun name -> { Name = name; Scoreboard = Scoreboard.filled() })
            |> List.sortByDescending (fun x -> x.Scoreboard.GetTotal())

    module DiceSet =
        let flat = DiceSet.init { NrOfDice = 6; Style = Style.Flat }
        let cube = DiceSet.init { NrOfDice = 6; Style = Style.Cube Even }

    let init scoreboard gameState hallOfFame =
        { DiceSet = DiceSet.flat
          Player = Player.random()
          Scoreboard = scoreboard
          GameState = gameState
          HallOfFame = hallOfFame }

module Storage =
    open Fable.Import
    open Thoth.Json

    let storage = Browser.WebStorage.localStorage
    let [<Literal>] GameStateKey = "jamb-gameState"
    let [<Literal>] HallOfFameKey = "jamb-hallOfFame"

    module GameState =
        let (encoder: Encoder<State>) = Encode.Auto.generateEncoder()
        let (decoder: Decoder<State>) = Decode.Auto.generateDecoder()

    module HallOfFame =
        let (encoder: Encoder<HallOfFameEntry list>) = Encode.Auto.generateEncoder()
        let (decoder: Decoder<HallOfFameEntry list>) = Decode.Auto.generateDecoder()

    let loadGameState () =
        storage.getItem GameStateKey
        |> Decode.fromString GameState.decoder
        |> function | Ok value -> Some value | _ -> None

    let saveGameState state =
        let json = GameState.encoder state |> Encode.toString 2
        storage.setItem(GameStateKey, json)

    let loadHallOfFame () =
        storage.getItem HallOfFameKey
        |> Decode.fromString HallOfFame.decoder
        |> function | Ok value -> Some value | _ -> None

    let saveHallOfFame state =
        let json = HallOfFame.encoder state |> Encode.toString 2
        storage.setItem(GameStateKey, json)

let initTest() =
    { DiceSet = Test.DiceSet.flat
      Player = Test.Player.random()
      Scoreboard = Test.Scoreboard.filled()
      GameState = GameOver
      HallOfFame = Test.HallOfFame.filled() }

let init () =
    //initTest(), Cmd.none
    let hallOfFame = Storage.loadHallOfFame() |> Option.defaultValue []
    let state =
        Storage.loadGameState()
        |> Option.defaultValue (State.Empty DiceSet.defaultConfig hallOfFame)
    state, Cmd.none

type Message =
| StartRolling
| DiceRolled
| DiceClicked of DiceId
| CellClicked of ColumnId*RowId
| NewGame
| Undo
| ToggleDiceStyle

let rollingTimer rollTime =
    Cmd.OfAsync.perform Async.Sleep rollTime (fun _ -> DiceRolled)

let rec turn gameState =
    match gameState with
    | WaitingForRoll | WaitingForRollWithUndo _ | GameOver -> 0
    | Rolling gameState -> turn gameState
    | RolledOnce | WaitingForCall | RolledOnceCalled _ -> 1
    | RolledTwice | RolledTwiceCalled _ -> 2
    | WaitingForScore -> 3

let possibleScores (scoreboard: Scoreboard) includeCall =
    [ Up; Down; Free; if includeCall then Call]
    |> List.collect (fun colId ->
        scoreboard.PossibleScores colId
        |> List.map (fun rowId -> colId, rowId))

let highlightedCells state =
    match state.GameState with
    | WaitingForRoll
    | WaitingForRollWithUndo _
    | GameOver
    | Rolling _
    | RolledOnceCalled _ -> List.empty

    | RolledOnce
    | WaitingForCall -> possibleScores state.Scoreboard true

    | RolledTwice
    | RolledTwiceCalled _
    | WaitingForScore -> possibleScores state.Scoreboard false

let saveToHallOfFame state =
    let newHallOfFame =
        [ yield! state.HallOfFame
          { Name = state.Player; Scoreboard = state.Scoreboard } ]
        |> List.sortByDescending (fun x -> x.Scoreboard.GetTotal())
        |> List.safeTake HallOfFameEntries

    Storage.saveHallOfFame newHallOfFame
    { state with HallOfFame = newHallOfFame }, Cmd.none

let newGame state =
    (State.Empty (DiceSet.config state.DiceSet) state.HallOfFame)

let score' state (colId, rowId) nextGameState =
    let dice = DiceSet.toDots state.DiceSet
    let scoreboard' = state.Scoreboard.Score (colId, rowId) dice
    let state' =
        { state with
            Scoreboard = scoreboard'
            DiceSet = DiceSet.unsaveAll state.DiceSet }

    let isGameOver = scoreboard'.IsFull

    if isGameOver then
        saveToHallOfFame { state' with GameState = GameOver }
    else
        { state' with GameState = nextGameState }, Cmd.none

let score state (colId, rowId) =
    let nextGameState = WaitingForRollWithUndo {| ScoredCell = (colId, rowId); UndoState = state.GameState |}
    score' state (colId, rowId) nextGameState

let tryScore state (colId, rowId) =
    let ignore = state, Cmd.none

    match state.GameState with
    | WaitingForRoll
    | WaitingForRollWithUndo _
    | GameOver
    | Rolling _ -> ignore
    | (RolledOnceCalled calledRow | RolledTwiceCalled calledRow)
        when (Call, calledRow) = (colId, rowId) ->
            score state (colId, rowId)
    | (RolledOnceCalled _ | RolledTwiceCalled _)->
        ignore
    | (RolledOnce | RolledTwice | WaitingForScore)
        when possibleScores state.Scoreboard false |> List.contains (colId, rowId) ->
            score state (colId, rowId)
    | (RolledOnce | RolledTwice | WaitingForScore ) ->
        ignore
    | WaitingForCall ->
        failwithf "State is in WaitingForCall but tried to score: %A" (colId, rowId)

let rec startRolling state =
    let ignore = state, Cmd.none

    let roll state =
        let diceSet, rollTime = DiceSet.rollNotSaved state.DiceSet
        { state with
            DiceSet = diceSet
            GameState = Rolling state.GameState }, rollingTimer rollTime

    match state.GameState with
    | WaitingForCall
    | WaitingForScore
    | Rolling _ ->
        ignore

    | GameOver ->
        roll (newGame state)

    | WaitingForRoll
    | WaitingForRollWithUndo _
    | RolledOnce
    | RolledOnceCalled _
    | RolledTwice
    | RolledTwiceCalled _ ->
        roll state


let diceRolled (state: State) =
    let ignore = state, Cmd.none

    let withNextGameState gs = { state with GameState = gs }, Cmd.none

    let nextState gs =
        match gs with
        | WaitingForRoll
        | WaitingForRollWithUndo _ ->
            let mustCall = possibleScores state.Scoreboard false |> List.isEmpty
            if mustCall
            then withNextGameState WaitingForCall
            else withNextGameState RolledOnce
        | GameOver ->
            withNextGameState RolledOnce
        | RolledOnce -> withNextGameState RolledTwice
        | RolledOnceCalled calledRow -> withNextGameState (RolledTwiceCalled calledRow)
        | RolledTwice -> withNextGameState WaitingForScore
        | RolledTwiceCalled calledRow -> score' state (Call, calledRow) WaitingForRoll

        | Rolling _ -> failwith "Rolling in Rolling GameState should not be possible"
        | WaitingForCall -> failwith "Rolling when WaitingForCall should not be possible."
        | WaitingForScore -> failwith "Rolling when WaitingForScore should not be possible."

    match state.GameState with
    | Rolling gs -> nextState gs
    | _ -> ignore

let toggleSave state diceId =
    { state with DiceSet = DiceSet.toggleSave diceId state.DiceSet }, Cmd.none

let cellClicked state (colId, rowId) =
    let ignore = state, Cmd.none

    let tryCall () =
        if state.Scoreboard.PossibleScores Call |> List.contains rowId
        then { state with GameState = RolledOnceCalled rowId }, Cmd.none
        else ignore

    let tryScore () = tryScore state (colId, rowId)

    match state.GameState, colId with
    | (WaitingForRoll | WaitingForRollWithUndo _ | Rolling _ | GameOver), _ -> ignore

    | WaitingForCall, Call -> tryCall ()
    | WaitingForCall, _ -> ignore

    | RolledOnce, Call -> tryCall ()
    | RolledOnce, _ -> tryScore ()

    | RolledOnceCalled calledRow, Call when calledRow = rowId -> tryScore ()
    | RolledOnceCalled _, Call -> tryCall ()
    | RolledOnceCalled _, _ -> ignore

    | RolledTwiceCalled calledRow, Call when calledRow = rowId -> tryScore ()
    | RolledTwiceCalled calledRow, _ -> ignore

    | (RolledTwice | WaitingForScore), Call -> ignore
    | (RolledTwice | WaitingForScore), _ -> tryScore ()

let undo state =
    let ignore = state, Cmd.none

    match state.GameState with
    | RolledOnceCalled _ ->
        let mustCall = possibleScores state.Scoreboard false |> List.isEmpty
        if mustCall
        then { state with GameState = WaitingForCall }, Cmd.none
        else { state with GameState = RolledOnce }, Cmd.none
    | WaitingForRollWithUndo x ->
        { state with
            Scoreboard = state.Scoreboard.Undo x.ScoredCell
            GameState = x.UndoState }, Cmd.none
    | _ -> ignore

let diceClicked state diceId =
    let ignore = state, Cmd.none

    match state.GameState with
    | Rolling _
    | WaitingForRollWithUndo _
    | WaitingForRoll
    | WaitingForRollWithUndo _ ->
        ignore
    | _ ->
        toggleSave state diceId

let toggleDiceStyle state =
    { state with DiceSet = DiceSet.toggleStyle state.DiceSet }, Cmd.none

let saveGameStateToLocalStorage (state, cmd) =
    Storage.saveGameState state
    state, cmd

let update message state =
    let ignore = state, Cmd.none

    match message with
    | NewGame -> newGame state, Cmd.none
    | StartRolling -> startRolling state
    | DiceRolled -> diceRolled state
    | DiceClicked diceId -> diceClicked state diceId
    | CellClicked (colId, rowId) -> cellClicked state (colId, rowId)
    | Undo -> undo state
    | ToggleDiceStyle -> toggleDiceStyle state
    |> saveGameStateToLocalStorage

let icon (faIcon: string) =
    Bulma.icon [
        Html.i [ prop.classes [ Fa.Fa; faIcon ] ] ]

let renderScoreboard (state: State) dispatch =
    let renderScoreboard highlight called =
        Scoreboard.render state.Scoreboard highlight called (CellClicked >> dispatch)

    match state.GameState with
    | WaitingForRoll | WaitingForRollWithUndo _ | Rolling _ | GameOver ->
        renderScoreboard [] None
    | RolledOnce | RolledTwice | WaitingForScore | WaitingForCall ->
        renderScoreboard (highlightedCells state) None
    | RolledOnceCalled calledRow | RolledTwiceCalled calledRow ->
        renderScoreboard [ Call, calledRow ] (Some calledRow)

let iconButton faIcon onClick =
    Bulma.button.button
        [ color.isDanger
          prop.children [ icon faIcon ]
          prop.onClick onClick ]

let renderRollComponent state dispatch =
    let renderTurn turn =
        let rollsLeft = 3 - turn
        if rollsLeft = 0
        then
            Html.div [ icon Fa.FaHourglass ]
        else
            [ Fa.FaDiceOne; Fa.FaDiceTwo; Fa.FaDiceThree ]
            |> List.map icon
            |> List.take rollsLeft
            |> Html.div

    let canUndo =
        match state.GameState with
        | WaitingForRollWithUndo _ | RolledOnceCalled _ -> true
        | _ -> false

    let undoButton =
        Bulma.column [
            column.is2
            prop.children [
                Bulma.button.button [
                    button.isFullWidth
                    button.isLarge
                    color.isWarning
                    prop.onClick (fun _ -> dispatch Undo)
                    prop.children [
                        Bulma.icon [ Html.i [ prop.classes [ Fa.Fa; Fa.FaUndo ] ] ] ] ] ] ]

    let isRolling =
        match state.GameState with
        | Rolling _ -> true
        | _ -> false

    let canRoll =
        match state.GameState with
        | WaitingForRoll
        | WaitingForRollWithUndo _
        | RolledOnce
        | RolledOnceCalled _
        | RolledTwice
        | RolledTwiceCalled _
        | GameOver -> true

        | WaitingForScore
        | WaitingForCall
        | Rolling _ -> false

    let renderRollButton faIcon =
        Bulma.button.a [
            prop.children [ icon faIcon ]
            color.isInfo
            button.isLarge
            button.isFullWidth
            prop.disabled (not canRoll)
            prop.onClick (fun _ -> dispatch StartRolling)
            if isRolling then button.isLoading
        ]

    let rollButton =
        match state.GameState with
        | WaitingForRoll -> renderRollButton Fa.FaDice
        | WaitingForRollWithUndo _ -> renderRollButton Fa.FaDice
        | Rolling _ -> renderRollButton Fa.FaDice
        | RolledOnce -> renderRollButton Fa.FaDice
        | WaitingForCall -> renderRollButton Fa.FaCheckCircle
        | RolledOnceCalled _ -> renderRollButton Fa.FaDice
        | RolledTwice -> renderRollButton Fa.FaDice
        | RolledTwiceCalled _ -> renderRollButton Fa.FaDice
        | WaitingForScore -> renderRollButton Fa.FaPencilAlt
        | GameOver -> renderRollButton Fa.FaTrophy

    let renderState =
        Html.div [
            color.isLight
            prop.text (sprintf "%A" state.GameState)
        ]

    Html.div
        [ Bulma.columns
            [ if canUndo then undoButton
              Bulma.column [ rollButton ] ]
          renderTurn (turn state.GameState)
          renderState ]

let diceStyleSwitch style dispatch =
    Bulma.navbarItem.div [
        Bulma.field.div [
            prop.onClick (fun _ -> dispatch ToggleDiceStyle)
            prop.children [
                Html.label [
                    prop.htmlFor "dice-style"
                    prop.children [ icon Fa.FaSquare; Html.text " " ] ]
                Switch.checkbox [
                    prop.id "dice.style"
                    switch.isRounded
                    prop.isChecked (style <> Flat)
                    color.isSuccess
                ]
                Html.label [
                    prop.htmlFor "dice-style"
                    prop.children [ icon Fa.FaCube ] ] ] ] ]

let navbarMenu (state: State) (dispatch: Message -> unit) =
    Bulma.navbarMenu [
        Bulma.navbarEnd.div [
            Bulma.navbarItem.div [
                diceStyleSwitch state.DiceSet.Style dispatch
                iconButton Fa.FaFastBackward (fun _ -> dispatch NewGame) ] ] ]

let renderDiceSet diceSet dispatch =
    Bulma.column [
        columns.isCentered
        prop.children [ DiceSet.render diceSet (DiceClicked >> dispatch) ] ]

let renderHallOfFame (hallOfFame: HallOfFame) dispatch =
    let head =
        Html.thead
            [ Html.tr
                [ Html.th "Place"
                  Html.th "Name"
                  Html.th "Score" ] ]

    let body =
        let rows =
            let emptyRow (place: int) =
                Html.tr
                    [ Html.th place
                      Html.th "-"
                      Html.td "-" ]
            let toRow (place: int) entry =
                Html.tr
                    [ Html.th place
                      Html.th entry.Name
                      Html.td (entry.Scoreboard.GetTotal()) ]

            [1..HallOfFameEntries]
            |> List.map (fun place ->
                List.tryItem (place-1) hallOfFame
                |> Option.map (toRow place)
                |> Option.defaultValue (emptyRow place))

        Html.tbody rows

    Bulma.table [
        table.isBordered
        table.isNarrow
        table.isFullWidth
        prop.children
            [ head
              body ]
    ]

let view (state: State) (dispatch: Message -> unit) =
    Bulma.columns [
        columns.isCentered
        prop.children [
            Bulma.column [
                column.is6
                prop.children [ Bulma.card [ renderScoreboard state dispatch ] ]
            ]
            Bulma.column [
                column.is4
                prop.children
                    [ renderHallOfFame state.HallOfFame dispatch
                      renderDiceSet state.DiceSet dispatch
                      renderRollComponent state dispatch ]
            ]
        ]
    ]
