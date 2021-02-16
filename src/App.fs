namespace Docs

module App =
    open Elmish
    open Elmish.React
    open Fable.React
    open Fable.React.Props
    open Fss
    open System
    
    let unCurry f (x, y) = f x y

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
    
    let boardCoords =
        [-5 .. 5] |> List.collect
            (fun i -> [-5 .. 5] |> List.map
                        (fun j -> i,j))
    
    type Piece =
        | King
        | King'sMan
        | Clan'sMan
        
    type Board = {
        king'sPosition: int * int
        king'sMen: (int * int) list
        clan'sMen: (int * int) list
    }
    
    let initBoard () = {
            king'sPosition = 0,0
            king'sMen = boardCoords |> List.filter (unCurry isKing'sManSquare)
            clan'sMen = boardCoords |> List.filter (unCurry isClan'sManSquare)
        }

    type Model = { boardState: Board }

    type Msg =
        | SetInput of string
        | AddTodo of string

    let init () = { boardState = initBoard () }

    let update (msg: Msg) (model: Model): Model =
        model

    let render ({ boardState = boardState }: Model) (dispatch: Msg -> unit) =
        let grid =
            fss [ Display.Grid
                  GridTemplateColumns.Repeat(11, px 50)
                  GridTemplateRows.Repeat(11, px 50) ]

        let square dark king'sSquare king'sManSquare clan'sManSquare isKing'sCastle =
            fss [ Position.Relative
                  
                  if dark then
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
            
        let kingPiece =
            fss [
                BackgroundColor.orangeRed
                Height' (pct 100)
                Width' (pct 100)
                BorderRadius' (pct 100)
            ]
            
        let piece king'sMan =
            fss [
                if king'sMan then
                    BackgroundColor.orangeRed
                else
                    BackgroundColor.black
                    
                Height' (pct 70)
                Width' (pct 70)
                BorderRadius' (pct 100)
                
                Position.Absolute
                Transforms [
                    Transform.TranslateX (pct -50)
                    Transform.TranslateY (pct -50)
                ]
                Left' (pct 50)
                Top' (pct 50)
            ]

        div [ ClassName grid ] <|
        (boardCoords |> List.map
            (fun (i,j) ->
                div [ ClassName(
                        square
                          (isDarkSquare i j)
                          (isKing'sSquare i j)
                          (isKing'sManSquare i j)
                          (isClan'sManSquare i j)
                          (isKing'sCastle i j) ) ]
                    [
                        if boardState.king'sPosition = (i, j) then
                            div [ ClassName kingPiece ] []
                            
                        if boardState.king'sMen |> List.contains (i, j) then
                            div [ ClassName (piece true) ] []
                            
                        if boardState.clan'sMen |> List.contains (i, j) then
                            div [ ClassName (piece false) ] []
                    ]))

    Program.mkSimple init update render
    |> Program.withReactSynchronous "elmish-app"
    |> Program.run
