module Jamb.Scoreboard

open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
open Zanaptak.TypedCssClasses

type Fa = CssClasses<"../node_modules/@fortawesome/fontawesome-free/css/fontawesome.min.css", Naming.PascalCase>

type DiceDots = int

type Cell =
| NotPlayed
| Played of DiceDots list

module Cell =
    let takeDiceCount = 5

    let sumOfDots diceDots =
        List.sum diceDots

    let numberOfDots (nrOfDots: int) (diceDots: DiceDots list) =
        diceDots
        |> List.filter (fun dots -> dots = nrOfDots)
        |> sumOfDots
        |> min (nrOfDots*takeDiceCount)

    let maxDotsWithN n (dice: DiceDots list) =
        dice
        |> List.countBy id
        |> List.filter (fun (_dots, count) -> count >= n)
        |> List.map fst
        |> List.sortByDescending id
        |> List.tryHead

    let private nOfAKind (n: int) bonus (dice: DiceDots list) =
        maxDotsWithN n dice
        |> Option.map (fun dots -> (dots * n) + bonus)
        |> Option.defaultValue 0

    let threeOfAKind = nOfAKind 3
    let fourOfAKind = nOfAKind 4
    let yahtzee = nOfAKind 5

    let fullHouse bonus (dice: DiceDots list) =
        maxDotsWithN 3 dice
        |> Option.bind (fun three ->
            let rest = dice |> List.filter (fun dots -> dots <> three)
            maxDotsWithN 2 rest |> Option.map (fun two -> three*3 + two*2 + bonus))
        |> Option.defaultValue 0

    let private straight straights bonus (dice: DiceDots list) =
        let diceValues = dice |> set

        let pickStraight straightValues =
            if Set.intersect straightValues diceValues = straightValues then
                Set.toList straightValues
                |> List.sum
                |> (+) bonus
                |> Some
            else
                None

        straights
        |> List.map set
        |> List.tryPick pickStraight
        |> Option.defaultValue 0

    let smallStraight = straight [[3..6]; [2..5]; [1..4]]
    let largeStraight = straight [[2..6]; [1..5]]

    let private minMax (sort: int list -> int list) (dice: DiceDots list) =
        dice
        |> sort
        |> List.take takeDiceCount
        |> List.sum

    let minimum = minMax List.sort
    let maximum = minMax List.sortDescending

type RowId = Ones | Twos | Threes | Fours | Fives | Sixes | Max | Min | ThreeOfAKind | FullHouse | LargeStraight | FourOfAKind | Yahtzee
let upperSectionRows = [ Ones; Twos; Threes; Fours; Fives; Sixes ]
let lowerSectionRows = [ ThreeOfAKind; FullHouse; LargeStraight; FourOfAKind; Yahtzee ]
let rowIds =
    List.concat [
        upperSectionRows
        [ Max; Min ]
        lowerSectionRows ]

type SumId = UpperSection | MinMaxTimesOnes | LowerSection | ColumnTotal

type CalledCell = RowId option

type ColumnId = Down | Up | Free | Call
let columnIds = [ Down; Up; Free; Call ]

type CellId = ColumnId * RowId

type Scoreboard =
    Scoreboard of Map<CellId, Cell>
    with
        static member Init () =
            columnIds
            |> List.collect (fun colId -> rowIds |> List.map (fun rowId -> (colId, rowId), NotPlayed))
            |> Map.ofList
            |> Scoreboard

        member sb.Cells = let (Scoreboard cells) = sb in cells

        member sb.IsFull =
            Map.forall (fun _key cell -> cell <> NotPlayed ) sb.Cells

        member sb.GetScore (colId, rowId) =
            match sb.Cells.[colId, rowId] with
            | NotPlayed -> None
            | Played d ->
                match rowId with
                | Ones -> Cell.numberOfDots 1 d
                | Twos -> Cell.numberOfDots 2 d
                | Threes -> Cell.numberOfDots 3 d
                | Fours -> Cell.numberOfDots 4 d
                | Fives -> Cell.numberOfDots 5 d
                | Sixes -> Cell.numberOfDots 6 d
                | Max -> Cell.maximum d
                | Min -> Cell.minimum d
                | ThreeOfAKind -> Cell.threeOfAKind 10 d
                | FullHouse -> Cell.fullHouse 20 d
                | LargeStraight -> Cell.largeStraight 30 d
                | FourOfAKind -> Cell.fourOfAKind 40 d
                | Yahtzee -> Cell.yahtzee 50 d
                |> Some

        member sb.GetSum (colId, sumId) =
            let sumColumnRows colId rows =
                rows
                |> List.sumBy (fun rowId -> sb.GetScore(colId, rowId) |> Option.defaultValue 0)

            match sumId with
            | UpperSection ->
                let sum = sumColumnRows colId upperSectionRows
                if sum >= 60 then sum + 30 else sum
            | LowerSection -> sumColumnRows colId lowerSectionRows
            | MinMaxTimesOnes ->
                let max = sb.GetScore(colId, Max)
                let min = sb.GetScore(colId, Min)
                let ones = sb.GetScore(colId, Ones)
                Option.map3 (fun ones max min -> ones*(max - min)) ones max min
                |> Option.defaultValue 0
            | ColumnTotal _ ->
                [ UpperSection; MinMaxTimesOnes; LowerSection ]
                |> List.sumBy (fun sumId -> sb.GetSum(colId, sumId))

        member sb.GetTotal() =
            [Up; Down; Free; Call]
            |> List.sumBy (fun colId -> sb.GetSum(colId, ColumnTotal))

        member sb.NotPlayed colId =
            rowIds
            |> List.filter (fun rowId -> sb.Cells.[colId, rowId] = NotPlayed)

        member sb.PossibleScores colId =
            match colId with
            | Down ->
                rowIds
                |> List.tryFind (fun rowId -> sb.Cells.[Down, rowId] = NotPlayed)
                |> Option.map (fun rowId -> [ rowId ])
                |> Option.defaultValue List.empty
            | Up ->
                rowIds
                |> List.rev
                |> List.tryFind (fun rowId -> sb.Cells.[Up, rowId] = NotPlayed)
                |> Option.map (fun rowId -> [ rowId ])
                |> Option.defaultValue List.empty
            | Free ->
                sb.NotPlayed Free
            | Call ->
                sb.NotPlayed Call

        member sb.Score (colId, rowId) dice =
            sb.Cells
            |> Map.add (colId, rowId) (Played dice)
            |> Scoreboard

        member sb.Undo (colId, rowId) =
            sb.Cells
            |> Map.add (colId, rowId) NotPlayed
            |> Scoreboard

type RowType =
| CellRow of RowId
| SumRow of SumId
| ColTotal

let render (scoreBoard: Scoreboard) possibleScores calledCell onCellClicked =
    let cols = [ Down; Up; Free; Call ]

    let icon faIcon =
        Bulma.icon [ prop.classes [ Fa.Fas; faIcon ] ]

    let renderCell colId row =
        match row with
        | CellRow rowId ->
            let score = scoreBoard.GetScore (colId, rowId)
            let renderScore  =
                score
                |> Option.map (function | 0 -> icon Fa.FaTimes | value -> Html.text value)
                |> Option.defaultValue (Html.text " ")
            let canBeScored = possibleScores |> List.contains (colId, rowId)
            let isCalled = colId = Call && calledCell = Some rowId

            Html.td [
                prop.children renderScore
                prop.style
                    [ if canBeScored then style.cursor.pointer else style.cursor.notAllowed
                      if canBeScored then style.backgroundColor.beige
                      if score = Some 0 then style.backgroundColor.lightCoral
                      if isCalled then style.backgroundColor.orange
                      style.textAlign.center ]
                if canBeScored then prop.onClick (fun _ -> onCellClicked (colId, rowId)) ]
        | SumRow sumId ->
            Html.td [
                prop.style [ style.textAlign.center ]
                color.isWarning
                prop.text (scoreBoard.GetSum (colId, sumId))
            ]
        | ColTotal ->
            Html.td [
                prop.style [ style.textAlign.center ]
                color.isSuccess
                prop.text (scoreBoard.GetSum (colId, ColumnTotal))
            ]

    let renderRow (rowHeader: ReactElement) optColor (row: RowType) =
        Html.tr [
            Html.th [
                prop.children [ rowHeader ]
                match optColor with
                | Some color -> color
                | None -> ()
                prop.style [ style.textAlign.right ]
            ]
            yield! cols |> List.map (fun colId -> renderCell colId row)
        ]

    let renderCellRow (rowHeader: string) (rowId: RowId) =
        renderRow (Html.span rowHeader) None (CellRow rowId)

    let renderSumRow (sumId: SumId) =
        renderRow (icon "fa-plus") None (RowType.SumRow sumId)

    let rows = [
        renderCellRow "1" RowId.Ones
        renderCellRow "2" RowId.Twos
        renderCellRow "3" RowId.Threes
        renderCellRow "4" RowId.Fours
        renderCellRow "5" RowId.Fives
        renderCellRow "6" RowId.Sixes
        renderSumRow UpperSection
        renderCellRow "Max" RowId.Max
        renderCellRow "Min" RowId.Min
        renderSumRow MinMaxTimesOnes
        renderCellRow "Tris"  RowId.ThreeOfAKind
        renderCellRow "Full"  RowId.FullHouse
        renderCellRow "Skala" RowId.LargeStraight
        renderCellRow "Poker" RowId.FourOfAKind
        renderCellRow "Jamb"  RowId.Yahtzee
        renderSumRow LowerSection
        renderRow (icon "fa-equals") None (RowType.ColTotal )
    ]

    Bulma.table [
        table.isFullWidth
        table.isBordered
        table.isStriped
        size.isSize5
        prop.children [
            Html.thead [
                let header (optIcon: string option) =
                    Html.th [
                        match optIcon with
                        | Some faIcon -> prop.children [ Bulma.icon [ Bulma.icon.isMedium; prop.classes ["fas"; faIcon] ] ]
                        | _ -> ()
                        prop.style [ style.textAlign.center ] ]
                Html.tr [
                    header None
                    header <| Some "fa-long-arrow-alt-down"
                    header <| Some "fa-long-arrow-alt-up"
                    header <| Some "fa-arrows-alt-v"
                    header <| Some "fa-check"
                ]
            ]
            Html.tableBody [
                yield! rows
                Html.tr [
                    Html.th [
                        prop.children [
                            Bulma.icon [
                                Bulma.icon.isLarge
                                prop.className "fas fa-trophy"
                            ]
                        ]
                        color.isInfo
                        prop.style [ style.textAlign.right ]
                    ]
                    Html.td [
                        prop.colSpan 4
                        size.isSize3
                        ++ color.isInfo
                        prop.style [ style.textAlign.center ]
                        prop.text (scoreBoard.GetTotal())
                    ]
                ]
            ]
        ]
    ]
