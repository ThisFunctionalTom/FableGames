module Home.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Types

let card icon heading body link =
  Column.column [ Column.Width (Screen.All, Column.Is4) ]
    [ Card.card [ ]
        [ Card.image
            [ Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ Icon.icon [ Icon.Size IsMedium
                          Icon.Props [ Style [ MarginTop "15px" ] ] ]
                [ Fa.i [icon; Fa.IconOption.Size Fa.Fa2x] [] ] ]
          Card.content [ ]
            [ Content.content [ ]
                [ h4 [ ] [ str heading ]
                  p [ ] [ str body ]
                  p [ ]
                    [ a [ Href ("#" + link) ]
                        [ str "Play" ] ] ] ] ] ]

let games =
    Bulma.columns [
        prop.className "features"
        prop.children [
            card Fa.Solid.Dice "Jamb" "Jamb is a dice game like Yahtzee but with somewhat different rules." "jamb"
            card Fa.Solid.Dice "Tetirs" "TODO: Write description" "tetris" ] ]

let intro =
    Bulma.column [
        prop.className "intro"
        column.isHalfDesktop
        column.is8
        column.isOffset2
        prop.children [
            h2 [ ClassName "title" ] [ str "Enjoy playing one of the games!" ]
            br [ ]
            p [ ClassName "subtitle"]
              [ str """I made this game for fun. I hope you will enjoy playing them.
                     Also, if you like to see how it is implemented go here.""" ] ] ]

let root model dispatch =
  Html.div [ intro; games ]
