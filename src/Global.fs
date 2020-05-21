module Global

type Page =
    | Home
    | Jamb

module PageHash =
    let [<Literal>] Home = "home"
    let [<Literal>] Jamb = "jamb"

let toHash page =
    match page with
    | Jamb -> PageHash.Jamb
    | Home -> PageHash.Home
    |> sprintf "#%s"
