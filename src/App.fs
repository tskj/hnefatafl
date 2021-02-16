namespace Docs

module App =
    open Elmish
    open Elmish.React
    open Fable.React
    open Fable.React.Props
    open Fss
    open System

    type Model = { Input: string; Todos: string list }

    type Msg =
        | SetInput of string
        | AddTodo of string

    let init () = { Input = ""; Todos = [] }

    let update (msg: Msg) (model: Model): Model =
        match msg with
        | SetInput input -> { model with Input = input }
        | AddTodo todo ->
            { model with
                  Todos = model.Todos @ [ todo ]
                  Input = "" }

    let render (model: Model) (dispatch: Msg -> unit) =
        let grid =
            fss [ Display.Grid
                  GridTemplateColumns.Repeat(11, px 50)
                  GridTemplateRows.Repeat(11, px 50) ]

        let square dark king'sSquare king'sManSquare clan'sManSquare isKing'sCastle =
            fss [ if dark then
                      BackgroundColor.gainsboro
                  else
                      BackgroundColor.white

                  if king'sSquare then
                      BackgroundColor.black

                  if king'sManSquare then
                      BackgroundColor.darkGray

                  if clan'sManSquare then
                      BackgroundColor.rebeccaPurple

                  if isKing'sCastle then
                      BackgroundColor.lime
            ]

        let isDarkSquare i j = (i + j) % 2 = 0
        let isKing'sSquare i j = i = 0 && j = 0

        let isKing'sManSquare (i: int) (j: int) =
            let i' = Math.Abs i
            let j' = Math.Abs j

            [ (0, 1)
              (0, 2)
              (1, 0)
              (2, 0)
              (1, 1) ]
            |> List.contains (i', j')

        let isClan'sManSquare (i: int) (j: int) =
            let i' = Math.Abs i
            let j' = Math.Abs j

            [ (0, 4)
              (0, 5)
              (1, 5)
              (2, 5)
              (4, 0)
              (5, 0)
              (5, 1)
              (5, 2) ]
            |> List.contains (i', j')

        let isKing'sCastle i j = (i = 5 || i = -5) && (j = 5 || j = -5)

        div [ ClassName grid ] <|
            ([ -5 .. 5 ] |> List.collect
                (fun i -> [ -5 .. 5 ] |> List.map
                            (fun j ->
                                div [ ClassName(
                                        square
                                          (isDarkSquare i j)
                                          (isKing'sSquare i j)
                                          (isKing'sManSquare i j)
                                          (isClan'sManSquare i j)
                                          (isKing'sCastle i j) ) ] [])))

    Program.mkSimple init update render
    |> Program.withReactSynchronous "elmish-app"
    |> Program.run
