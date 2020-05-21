module Home.View

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
open Zanaptak.TypedCssClasses

type Fa = CssClasses<"../node_modules/@fortawesome/fontawesome-free/css/fontawesome.min.css", Naming.PascalCase>

let icon faIcon =
    Bulma.icon [
        prop.children [
            Html.i [
                prop.style [ style.marginTop 30 ]
                prop.classes [ Fa.Fa; faIcon; Fa.Fa2X ] ] ] ]

let card image (heading: string) (body: string) optLink =
  Column.column [ Column.Width (Screen.All, Column.Is4) ]
    [ Card.card [ ]
        [ Card.image
            [ Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ image ]
          Card.content [ ]
            [ Content.content [ ]
                [ Bulma.title.h4 heading
                  Html.p body
                  Html.p
                    [ match optLink with
                      | Some link -> Html.a [ prop.href ("#" + link); prop.text "Play" ]
                      | None -> Html.text "[ work in progress ]" ] ] ] ] ]

let games =
    Bulma.columns [
        prop.className "features"
        prop.children [
            card (icon Fa.FaDice) "Jamb" "Jamb is a dice game like Yahtzee but with somewhat different rules." (Some "jamb")
            card (icon Fa.FaCubes) "Tetris" "TODO: Write description" None ] ]

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

let view =
  Html.div [ intro; games ]
