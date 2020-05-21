module Navbar.View

open Fable.React
open Fable.React.Props
open Fulma
open Feliz
open Feliz.Bulma
open Fable.FontAwesome
open Zanaptak.TypedCssClasses

type Fa = CssClasses<"../node_modules/@fortawesome/fontawesome-free/css/fontawesome.min.css", Naming.PascalCase>

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "#home"
            size.isSize4
            prop.children [
                Bulma.icon [
                    prop.classes [ Fa.Fa; Fa.FaGamepad]
                    prop.style [ style.width (length.em 2) ]
                ]
                Html.text "Fable Games" ] ]
            ]

let root navbarMenu =
    Bulma.navbar [
        color.isInfo
        prop.children [
            Bulma.container [
                Bulma.navbarBrand.div [ navBrand ]
                navbarMenu ] ] ]
