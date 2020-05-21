module Jamb.Game

open System
open Elmish
open Scoreboard
open DiceSet
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Switch
open Zanaptak.TypedCssClasses

type Fa = CssClasses<"../node_modules/@fortawesome/fontawesome-free/css/fontawesome.min.css", Naming.PascalCase>

type RollingState = {
    CalledRow: RowId option
    Turn: int
    Rolled: bool
}

type GameState =
| WaitingForRoll of {| ScoredCell: CellId; UndoState: GameState |} option
| Rolling of RollingState
| WaitingForScore
| WaitingForCall of RollingState
| GameOver

type State = {
    DiceSet: DiceSet
    Scoreboard: Scoreboard
    GameState: GameState
} with
    static member Empty config =
        { DiceSet = DiceSet.init config
          Scoreboard = Scoreboard.Init()
          GameState = WaitingForRoll None }

    static member Test =
        let sixes = List.replicate 6 6
        { DiceSet = DiceSet.init { NrOfDice = 6; Style = Style.Flat }
          Scoreboard =
            [ Up; Down; Free ]
            |> List.collect (fun colId -> rowIds |> List.map (fun rowId -> (colId, rowId), Played sixes))
            |> List.append (rowIds |> List.map (fun rowId -> (Call, rowId), NotPlayed))
            |> Map.ofList
            |> Scoreboard
          GameState = WaitingForRoll None }

module Storage =
    open Fable.Import
    open Thoth.Json

    let storage = Browser.WebStorage.localStorage
    let [<Literal>] StorageKey = "jamb"

    let (encoder: Encoder<State>) = Encode.Auto.generateEncoder()
    let (decoder: Decoder<State>) = Decode.Auto.generateDecoder()

    let load () =
        storage.getItem StorageKey
        |> Decode.fromString decoder
        |> function | Ok value -> Some value | _ -> None

    let save state =
        let json = encoder state |> Encode.toString 2
        storage.setItem(StorageKey, json)

let init () =
    let state =
        Storage.load()
        |> Option.defaultValue (State.Empty DiceSet.defaultConfig)
    state, Cmd.none

type Message =
| StartRolling
| DiceStopped
| DiceClicked of DiceId
| CellClicked of ColumnId*RowId
| NewGame
| ToggleDiceStyle

let rollingTimer rollTime =
    Cmd.OfAsync.perform Async.Sleep rollTime (fun _ -> DiceStopped)

module GameState =
    let firstRoll =
        Rolling {
            CalledRow = None
            Turn = 1
            Rolled = false
        }

let possibleScores state =
    let possible includeCall =
        [ Up; Down; Free; if includeCall then Call]
        |> List.collect (fun colId ->
            state.Scoreboard.PossibleScores colId
            |> List.map (fun rowId -> colId, rowId))

    match state.GameState with
    | WaitingForScore ->
        possible false

    | Rolling { Rolled = false } ->
        []
    | Rolling { Rolled = true; Turn = 1 } | WaitingForCall { Rolled = true; Turn = 1 } ->
        possible true
    | Rolling { Rolled = true; CalledRow = None } ->
        possible false
    | Rolling { Rolled = true; CalledRow = Some rowId } ->
        [ Call, rowId ]

    | WaitingForCall { Rolled = false } ->
        []
    | WaitingForCall { Rolled = true } ->
        []
    | WaitingForRoll (Some x) ->
        [ x.ScoredCell ]

    | WaitingForRoll None ->
        []

    | GameOver ->
        []


let tryScore state (colId, rowId) =
    let ignore = state, Cmd.none

    if possibleScores state |> List.contains (colId, rowId) then
        let dice = DiceSet.toDots state.DiceSet
        let scoreboard' = state.Scoreboard.Score (colId, rowId) dice
        let gameState =
            if scoreboard'.IsFull
            then GameOver
            else WaitingForRoll (Some {| ScoredCell = (colId, rowId); UndoState = state.GameState |})
        { state with
            GameState = gameState
            Scoreboard = scoreboard'
            DiceSet = DiceSet.unsaveAll state.DiceSet }, Cmd.none
    else
        ignore


let rec startRolling state =
    let ignore = state, Cmd.none

    match state.GameState with
    | WaitingForRoll _ ->
        let firstRoll = Rolling { Turn = 1; Rolled = false; CalledRow = None }
        let diceSet, rollTime = DiceSet.rollAll state.DiceSet
        { state with
            DiceSet = diceSet
            GameState = firstRoll }, rollingTimer rollTime
    | Rolling x ->
        if not x.Rolled then
            ignore
        else
            let gameState = Rolling { x with Turn = x.Turn + 1; Rolled = false }
            let diceSet, rollTime = DiceSet.rollNotSaved state.DiceSet
            { state with
                DiceSet = diceSet
                GameState = gameState }, rollingTimer rollTime
    | WaitingForScore | WaitingForCall _ ->
        ignore
    | GameOver ->
        startRolling (State.Empty (DiceSet.config state.DiceSet))

let diceStopped state =
    let ignore = state, Cmd.none

    match state.GameState with
    | WaitingForRoll _ | WaitingForScore | GameOver | WaitingForCall _ ->
        ignore
    | Rolling x ->
        let rolled = { x with Rolled = true }
        let state' = { state with GameState = Rolling rolled }
        let onlyCallCellLeft = possibleScores state' |> List.map fst |> List.forall ((=) Call)
        printfn "Rolling: %A" x
        printfn "PossibleScores: %A" (possibleScores state')
        printfn "OnlyCallCellLeft: %b" onlyCallCellLeft
        match onlyCallCellLeft, x.CalledRow, x.Turn with
        | true, None, _ ->
            { state with GameState = WaitingForCall rolled }, Cmd.none
        | _, Some rowId, turn when turn >= 3 ->
            tryScore { state with GameState = Rolling rolled } (Call, rowId)
        | _, None, turn when turn >= 3 ->
            { state with GameState = WaitingForScore }, Cmd.none
        | _ ->
            { state with GameState = Rolling rolled }, Cmd.none


let toggleSave state diceId =
    { state with DiceSet = DiceSet.toggleSave diceId state.DiceSet }, Cmd.none

let cellClicked state (colId, rowId) =
    let ignore = state, Cmd.none

    match state.GameState with
    | GameOver ->
        ignore
    | WaitingForScore ->
        tryScore state (colId, rowId)
    | WaitingForRoll (Some x) when x.ScoredCell = (colId, rowId) ->
        { state with Scoreboard = state.Scoreboard.Undo x.ScoredCell; GameState = x.UndoState }, Cmd.none
    | WaitingForRoll _ ->
        ignore
    | Rolling x when x.Turn = 1 && x.CalledRow = Some rowId && colId = Call ->
        { state with GameState = Rolling { x with CalledRow = None } }, Cmd.none
    | Rolling x | WaitingForCall x ->
        match colId, x.Turn with
        | Call, 1 ->
            { state with GameState = Rolling { x with CalledRow = Some rowId } }, Cmd.none
        | _ ->
            tryScore state (colId, rowId)

let diceClicked state diceId =
    let ignore = state, Cmd.none

    match state.GameState with
    | WaitingForRoll _
    | Rolling { Rolled = false } ->
        ignore
    | _ ->
        toggleSave state diceId

let toggleDiceStyle state =
    { state with DiceSet = DiceSet.toggleStyle state.DiceSet }, Cmd.none

let saveStateToLocalStorage (state, cmd) =
    Storage.save state
    state, cmd

let update message state =
    let ignore = state, Cmd.none

    match message with
    | NewGame -> (State.Empty (DiceSet.config state.DiceSet)), Cmd.none
    | StartRolling -> startRolling state
    | DiceStopped -> diceStopped state
    | DiceClicked diceId -> diceClicked state diceId
    | CellClicked (colId, rowId) -> cellClicked state (colId, rowId)
    | ToggleDiceStyle -> toggleDiceStyle state
    |> saveStateToLocalStorage

let icon (faIcon: string) =
    Bulma.icon [
        Html.i [ prop.classes [ Fa.Fa; faIcon ] ] ]

let renderScoreboard (state: State) dispatch =
    match state.GameState with
    | WaitingForScore | WaitingForRoll _ ->
        Scoreboard.render state.Scoreboard (possibleScores state) None (CellClicked >> dispatch)
    | Rolling x | WaitingForCall x ->
        printfn "Render Scoreboard State: %A" state.GameState
        printfn "Render Scoreboard Possible Scores: %A" (possibleScores state)
        Scoreboard.render state.Scoreboard (possibleScores state) x.CalledRow (CellClicked >> dispatch)
    | GameOver ->
        Scoreboard.render state.Scoreboard [] None (CellClicked >> dispatch)

let renderGameOver score =
    renderScoreboard

let rollButton rolling canRoll visible faIcon dispatch =
    Bulma.button.a [
        prop.children [ icon faIcon ]
        button.isLarge
        button.isFullWidth
        if canRoll then button.isActive
        prop.disabled (not canRoll)
        if not visible then prop.className "is-hidden"
        if rolling then button.isLoading
        color.isInfo
        if canRoll then prop.onClick (fun _ -> dispatch StartRolling)
    ]

let renderRollButton state dispatch =
    let rollingButton rolled visible = rollButton (not rolled) rolled visible "fa-dice" dispatch
    let waitingForScoreButton visible = rollButton false false visible "fa-pen" dispatch
    let waitingForRollButton visible = rollButton false true visible "fa-dice" dispatch
    let waitingForCallButton visible = rollButton false false visible "fa-check" dispatch
    let gameOverButton visible = rollButton false true visible "fa-trophy" dispatch

    let buttons rolled r wfs wfr wfc go =
        Html.div [
            rollingButton rolled r
            waitingForScoreButton wfs
            waitingForRollButton wfr
            waitingForCallButton wfc
            gameOverButton go
        ]

    match state.GameState with
    | Rolling x -> buttons x.Rolled true false false false false
    | WaitingForScore -> buttons false false true false false false
    | WaitingForRoll _ -> buttons false false false true false false
    | WaitingForCall x -> buttons x.Rolled false false true false false
    | GameOver -> buttons false false false false false true

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
                Bulma.button.button [
                    color.isDanger
                    prop.children [ icon Fa.FaFastBackward ]
                    prop.onClick (fun _ -> dispatch NewGame) ] ] ] ]

let view (state: State) (dispatch: Message -> unit) =
    Bulma.columns [
        columns.isVCentered
        columns.isCentered
        prop.children [
            Bulma.column [
                column.is6
                prop.children [ Bulma.card [ renderScoreboard state dispatch ] ]
            ]
            Bulma.column [
                column.is4
                prop.children
                    [ Bulma.column [
                        columns.isCentered
                        prop.children [ DiceSet.render state.DiceSet (DiceClicked >> dispatch) ] ]
                      renderRollButton state dispatch ]
            ]
        ]
    ]
