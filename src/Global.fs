module Global

type Page =
    | Home
    | Jamb
    | About

module PageHash =
    let [<Literal>] Home = "home"
    let [<Literal>] Jamb = "jamb"
    let [<Literal>] About = "about"

let toHash page =
    match page with
    | About -> PageHash.About
    | Jamb -> PageHash.Jamb
    | Home -> PageHash.Home
    |> sprintf "#%s"
