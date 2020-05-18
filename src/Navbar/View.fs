module Navbar.View

open Fable.React
open Fable.React.Props
open Fulma
open Feliz
open Feliz.Bulma
open Fable.FontAwesome
open Zanaptak.TypedCssClasses

type Fa = CssClasses<"../node_modules/@fortawesome/fontawesome-free/css/fontawesome.min.css", Naming.PascalCase>
type b = CssClasses<"../node_modules/bulma/css/bulma.min.css", Naming.PascalCase>

let icon faIcon =
    Bulma.icon [
        Html.i [ prop.classes [ Fa.Fa; faIcon ] ]
    ]

let navButton (href: string) faIcon (txt: string) =
    Bulma.button.a [
        color.isWhite
        button.isOutlined
        prop.href href
        prop.children [
            icon faIcon
            Html.span txt ]
    ]

let navButtons' =
    Html.span
        [ prop.classes [ b.NavbarItem ]
          prop.children [
            Html.div [
                prop.classes [ b.Field; b.IsGrouped ]
                prop.children [ navButton "#jamb" Fa.FaDice "Jamb" ] ] ] ]

let navButtons =
    Bulma.navbarItem.div [
        Bulma.field.div [
                prop.children [
                navButton "#jamb" Fa.FaDice "Jamb"
                navButton "#tetris" Fa.FaCubes "Tetris" ] ] ]

let navBrand =
    Html.a [
        prop.href "#home"
        prop.classes [ b.NavbarItem; b.Title; b.Is4 ]
        prop.text "Fable Games" ]

let root currentPage =
    Bulma.navbar [
        color.isInfo
        prop.children [
            Bulma.container [
                Bulma.navbarBrand.div [ navBrand ]
                Bulma.navbarMenu [
                    Bulma.navbarStart.div navButtons ] ] ] ]
