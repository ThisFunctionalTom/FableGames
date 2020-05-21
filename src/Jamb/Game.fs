module Jamb.Game

open System
open Elmish
open Scoreboard
open DiceSet
open Feliz
open Feliz.Bulma

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
        { DiceSet = DiceSet.init { NrOfDice = 5; Style = Style.Flat }
          Scoreboard =
            [ Up; Down; Free ]
            |> List.collect (fun colId -> rowIds |> List.map (fun rowId -> (colId, rowId), Played sixes))
            |> List.append (rowIds |> List.map (fun rowId -> (Call, rowId), NotPlayed))
            |> Map.ofList
            |> Scoreboard
          GameState = WaitingForRoll None }

let init () =
    State.Empty DiceSet.defaultConfig, Cmd.none

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
    | Rolling { CalledRow = None; Turn = rollNr; Rolled = true } ->
        possible (rollNr = 1)
    | Rolling { CalledRow = Some rowId; Turn = rollNr; Rolled = true } ->
        [ Call, rowId ]
    | WaitingForRoll (Some x) ->
        [ x.ScoredCell ]
    | _ ->
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
    | Rolling x | WaitingForCall x ->
        match colId, x.CalledRow, x.Turn with
        | Call, None, 1 ->
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

let update message state =
    let ignore = state, Cmd.none

    match message with
    | NewGame -> (State.Empty (DiceSet.config state.DiceSet)), Cmd.none
    | StartRolling -> startRolling state
    | DiceStopped -> diceStopped state
    | DiceClicked diceId -> diceClicked state diceId
    | CellClicked (colId, rowId) -> cellClicked state (colId, rowId)
    | ToggleDiceStyle -> toggleDiceStyle state

let mediumIcon (faIcon: string) =
    Bulma.icon [
        //prop.id faIcon
        //icon.isMedium
        prop.classes ["fas"; faIcon ] ]

let renderScoreboard (state: State) dispatch =
    match state.GameState with
    | WaitingForScore | WaitingForRoll _ ->
        Scoreboard.render state.Scoreboard (possibleScores state) None (CellClicked >> dispatch)
    | Rolling x | WaitingForCall x ->
        Scoreboard.render state.Scoreboard (possibleScores state) x.CalledRow (CellClicked >> dispatch)
    | GameOver ->
        Scoreboard.render state.Scoreboard [] None (CellClicked >> dispatch)

let renderGameOver score =
    renderScoreboard

let rollButton rolling canRoll visible faIcon dispatch =
    Bulma.button.a [
        prop.children [ mediumIcon faIcon ]
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

let navbarMenu (state: State) (dispatch: Message -> unit) =
    Bulma.navbarMenu [
        Bulma.navbarStart.div [
            Bulma.navbarItem.div [
            Bulma.button.button [
                color.isLink
                prop.children [ mediumIcon "fa-cube" ]
                prop.onClick (fun _ -> dispatch ToggleDiceStyle) ] ] ]
        Bulma.navbarEnd.div [
            Bulma.navbarItem.div [
            Bulma.button.button [
                color.isDanger
                prop.children [ mediumIcon "fa-trash-alt" ]
                prop.onClick (fun _ -> dispatch NewGame) ] ] ] ]

let renderState state =
    Html.div [
        prop.text (sprintf "%A" state.GameState)
    ]

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
                      renderRollButton state dispatch
                      renderState state ]
            ]
        ]
    ]
