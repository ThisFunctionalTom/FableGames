module Jamb.DiceSet

open System
open Elmish
open Feliz
open Feliz.Bulma

type DiceId = int
type Color = string

type Roll = Even | Odd

type Style = Cube of Roll | Flat

let nextRoll = function
| Cube Even -> Cube Odd
| Cube Odd -> Cube Even
| Flat -> Flat

let diceColor = function
    | 1 -> "red"
    | 2 -> "green"
    | 3 -> "gray"
    | 4 -> "blue"
    | 5 -> "pink"
    | 6 -> "yellow"
    | _ -> "gray"

let random = Random()

type Dice = {
    Id: int
    Dots: int
    Color: Color
    Saved: bool
}

module Dice =
    let init id =
        { Id = id
          Dots = 6
          Color = diceColor id
          Saved = false }

    let roll (dice: Dice) =
        let dots = random.Next(1, 7)
        { dice with
            Dots = dots }

    let dots dice = dice.Dots

    let unsave dice =
        { dice with Saved = false }

    let toggleSave dice =
        { dice with Saved = not dice.Saved }

type DiceSet =
    { Dice: Dice list
      Style: Style }

type Config =
    { NrOfDice: int
      Style: Style }

let init config =
    { Dice = List.init config.NrOfDice (fun id -> Dice.init (id+1))
      Style = config.Style }

module DiceSet =
    let toDots (diceSet: DiceSet) =
        diceSet.Dice |> List.map Dice.dots

    let toggleStyle (diceSet: DiceSet) =
        let style' =
            match diceSet.Style with
            | Cube _ -> Flat
            | Flat -> Cube Even
        { diceSet with Style = style' }

    let config (diceSet: DiceSet) =
        { NrOfDice = diceSet.Dice.Length; Style = diceSet.Style }

    let defaultConfig = { NrOfDice = 6; Style = Style.Cube Even }

let getDots diceSet =
    diceSet.Dice
    |> List.map (fun d -> d.Dots)

let unsaveAll diceSet =
    { diceSet
        with
            Dice = diceSet.Dice |> List.map Dice.unsave }

let rollTime diceStyle =
    if diceStyle = Flat then 0 else 1250

let rollAll diceSet =
    { diceSet
        with
            Dice = diceSet.Dice |> List.map (Dice.unsave >> Dice.roll)
            Style = nextRoll diceSet.Style }, rollTime diceSet.Style

let rollNotSaved diceSet =
    { diceSet
        with
            Dice = diceSet.Dice |> List.map (fun d -> if d.Saved then d else Dice.roll d)
            Style = nextRoll diceSet.Style }, rollTime diceSet.Style

let toggleSave diceId diceSet =
    let dice =
        diceSet.Dice
        |> List.map (fun d -> if d.Id = diceId then Dice.toggleSave d else d )
    { diceSet with Dice = dice }

let face pos pipCount =
    let pip = Html.span [ prop.className "pip" ]
    Html.div [
        prop.classes  ["face"; pos]
        prop.children (List.replicate pipCount pip)
    ]

let render3dDice (state: Dice) (rollClass: string) onDiceClick =
    Html.div [
        prop.classes [
            "dice"
            rollClass
            if state.Saved then "saved"
            else state.Color ]
        prop.custom ("data-roll", state.Dots)
        prop.onClick (fun _ -> onDiceClick state.Id)
        prop.children [
            face "front" 1
            face "left" 2
            face "bottom" 3
            face "top" 4
            face "right" 5
            face "back" 6
        ]
    ]

let render2dDice (state: Dice) onDiceClick =
    Html.div [
        prop.classes [
            "dice"
            "classic"
            if state.Saved then "saved" ]
        prop.onClick (fun _ -> onDiceClick state.Id)
        prop.children [
            face "front" state.Dots
        ]
    ]

let render (state: DiceSet) onDiceClick =
    let styledDice dice =
        match state.Style with
        | Cube Even ->
            render3dDice dice "even-roll" onDiceClick
        | Cube Odd ->
            render3dDice dice "odd-roll" onDiceClick
        | Flat ->
            render2dDice dice onDiceClick

    Html.div [
        columns.isVCentered
        prop.children [
            Html.div [
                prop.classes [ "dice-set" ]
                prop.children [ for dice in state.Dice do styledDice dice ]
            ]
        ]
    ]

