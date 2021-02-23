namespace Docs

open Browser.Types

module App =
    open Elmish
    open Elmish.React
    open Fable.React
    open Fable.React.Props
    open Fss
    open System
    open Browser

    let uncurry f (x, y) = f x y

    let parse (s: string) =
        match Int32.TryParse s with
        | (true, i) -> Some i
        | _ -> None

    let toSet =
        List.collect
            (function
            | Some x -> [ x ]
            | None -> [])
        >> Set.ofList

    let replaceGuy (i, j) guy guys =
        guys
        |> List.map
            (function
            | Some (x, y) when x = i && y = j -> guy
            | x -> x)

    let removeGuys guys fromGuys =
        fromGuys
        |> List.map
            (function
            | Some (x, y) when (guys |> Set.contains (x, y)) -> None
            | x -> x)

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

    let isKing'sCastle (i, j) = (i = 5 || i = -5) && (j = 5 || j = -5)

    let boardCoords =
        [ -5 .. 5 ]
        |> List.collect (fun i -> [ -5 .. 5 ] |> List.map (fun j -> i, j))
        |> Set.ofList

    type Piece =
        | King
        | King'sMan
        | Clan'sMan

    type Board =
        { king'sPosition: int * int
          king'sMen: (int * int) option list
          clan'sMen: (int * int) option list }

    let initBoard () =
        { king'sPosition = 0, 0
          king'sMen =
              boardCoords
              |> List.ofSeq
              |> List.filter (uncurry isKing'sManSquare)
              |> List.map Some
          clan'sMen =
              boardCoords
              |> List.ofSeq
              |> List.filter (uncurry isClan'sManSquare)
              |> List.map Some }

    type Player =
        | KingSide
        | ClanSide


    type Model =
        { boardState: Board
          winner: Player option
          playerToMove: Player
          currentlyDragging: (Piece * int * int) option
          mousePos: double * double }

    type Msg =
        | Move of (Piece * (int * int) * (int * int))
        | DragStart of (Piece * int * int)
        | DragEnd
        | Capture of int * int
        | CheckWinner
        | MouseMove of double * double

    let init () =
        { boardState = initBoard ()
          winner = None
          playerToMove = ClanSide
          currentlyDragging = None
          mousePos = 0., 0. },
        Cmd.none


    let tee f x =
        f x
        x


    let legalMoves (gameState: Model) =
        let board = gameState.boardState

        match gameState.currentlyDragging, gameState.winner with
        | None, _ -> Set.empty
        | _, Some _ -> Set.empty
        | Some (piece, x, y), _ ->
            if (piece = King || piece = King'sMan)
               && gameState.playerToMove = ClanSide
               || piece = Clan'sMan
                  && gameState.playerToMove = KingSide then
                Set.empty
            else
                let isKing = board.king'sPosition = (x, y)

                let occupiedSquares =
                    board.king'sMen
                    |> toSet
                    |> Set.add board.king'sPosition
                    |> Set.union (board.clan'sMen |> toSet)

                let allPossibleMoves = boardCoords

                let rec squaresBetween (x: int, y: int) (i, j) =
                    let deltaX = Math.Sign(i - x)
                    let deltaY = Math.Sign(j - y)

                    if deltaX = 0 && deltaY = 0
                       || deltaX > 0 && x >= i
                       || deltaX < 0 && x <= i
                       || deltaY > 0 && y >= j
                       || deltaY < 0 && y <= j then
                        Set.empty
                    else
                        squaresBetween (x + deltaX, y + deltaY) (i, j)
                        |> Set.add (x + deltaX, y + deltaY)

                allPossibleMoves
                |> Set.filter
                    (fun (i, j) ->
                        // Remove occupied squares
                        occupiedSquares |> Set.contains (i, j) |> not)
                |> Set.filter
                    (fun (i, j) ->
                        // Remove all that are not horizontal or vertical
                        let deltaX = x - i
                        let deltaY = y - j
                        deltaX = 0 || deltaY = 0)
                |> Set.filter
                    (fun (i, j) ->
                        // Remove all occluded squares between start and end
                        let travelSquares = squaresBetween (x, y) (i, j)

                        let unoccupiedTravelSquares =
                            Set.difference travelSquares occupiedSquares

                        unoccupiedTravelSquares = travelSquares)
                |> Set.filter
                    (fun (i, j) ->
                        // Remove all the King's squares for everyone but the King
                        [ (5, 5)
                          (-5, -5)
                          (-5, 5)
                          (5, -5)
                          (0, 0) ]
                        |> List.contains (i, j)
                        |> not
                        || isKing)


    let enemyNeighbours (board: Board) (i, j) =
        let neighbours =
            [ (i, j + 1)
              (i, j - 1)
              (i + 1, j)
              (i - 1, j) ]
            |> Set.ofList

        let king'sPiece =
            board.king'sMen
            |> toSet
            |> Set.add board.king'sPosition
            |> Set.contains (i, j)

        let clan'sPiece =
            board.clan'sMen |> toSet |> Set.contains (i, j)

        if king'sPiece then
            board.clan'sMen
            |> toSet
            |> Set.filter (fun x -> neighbours |> Set.contains x)
        else if clan'sPiece then
            board.king'sMen
            |> toSet
            |> Set.add board.king'sPosition
            |> Set.filter (fun x -> neighbours |> Set.contains x)
        else
            Set.empty


    let togglePlayer playerToMove =
        match playerToMove with
        | KingSide -> ClanSide
        | ClanSide -> KingSide

    let capture (board: Board) (i, j) =
        let horizontalAttackVector = [ (i + 1, j); (i - 1, j) ] |> Set.ofList
        let verticalAttackVector = [ (i, j + 1); (i, j - 1) ] |> Set.ofList

        let enemyNeighbours' = enemyNeighbours board (i, j)

        let horizontalAttack =
            (horizontalAttackVector |> Set.isSubset) enemyNeighbours'

        let verticalAttack =
            (verticalAttackVector |> Set.isSubset) enemyNeighbours'

        horizontalAttack || verticalAttack

    let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
        match msg with
        | Move (piece, (fromX, fromY), (toX, toY)) ->
            let isLegalMove =
                legalMoves model |> Set.contains (toX, toY)

            if isLegalMove then
                match piece with
                | King ->
                    { model with
                          boardState =
                              { model.boardState with
                                    king'sPosition = (toX, toY) }
                          playerToMove = togglePlayer model.playerToMove },
                    Cmd.batch [ DragEnd |> Cmd.ofMsg
                                Capture(toX, toY) |> Cmd.ofMsg
                                CheckWinner |> Cmd.ofMsg ]
                | King'sMan ->
                    { model with
                          boardState =
                              { model.boardState with
                                    king'sMen =
                                        model.boardState.king'sMen
                                        |> replaceGuy (fromX, fromY) (Some(toX, toY)) }
                          playerToMove = togglePlayer model.playerToMove },
                    Cmd.batch [ DragEnd |> Cmd.ofMsg
                                Capture(toX, toY) |> Cmd.ofMsg
                                CheckWinner |> Cmd.ofMsg ]
                | Clan'sMan ->
                    { model with
                          boardState =
                              { model.boardState with
                                    clan'sMen =
                                        model.boardState.clan'sMen
                                        |> replaceGuy (fromX, fromY) (Some(toX, toY)) }
                          playerToMove = togglePlayer model.playerToMove },
                    Cmd.batch [ DragEnd |> Cmd.ofMsg
                                Capture(toX, toY) |> Cmd.ofMsg
                                CheckWinner |> Cmd.ofMsg ]
            else
                model, Cmd.none

        | DragStart data ->
            { model with
                  currentlyDragging = Some data },
            Cmd.none
        | DragEnd -> { model with currentlyDragging = None }, Cmd.none
        | Capture (i, j) ->
            let enemyNeighbours' = enemyNeighbours model.boardState (i, j)

            let capturedNeighbours =
                enemyNeighbours'
                |> Set.filter (capture model.boardState)

            { model with
                  boardState =
                      { model.boardState with
                            king'sMen =
                                model.boardState.king'sMen
                                |> replaceGuy (i, j) None
                            clan'sMen =
                                model.boardState.clan'sMen
                                |> removeGuys capturedNeighbours //(model.boardState.clan'sMen |> toSet |> Set.difference) capturedNeighbours
                      } },
            Cmd.none

        | CheckWinner ->
            let isKingWinner =
                isKing'sCastle model.boardState.king'sPosition

            let isClanWinner =
                enemyNeighbours model.boardState model.boardState.king'sPosition
                |> Set.count = 4

            { model with
                  winner =
                      if isKingWinner then Some KingSide
                      else if isClanWinner then Some ClanSide
                      else None },
            Cmd.none

        | MouseMove (x, y) ->
            match model.currentlyDragging with
            | None -> model, Cmd.none
            | Some _ -> { model with mousePos = (x, y) }, Cmd.none

        | _ -> model, Cmd.none

    let render (model: Model) (dispatch: Msg -> unit) =
        let grid =
            fss [ Display.Grid
                  GridTemplateColumns.Repeat(11, px 50)
                  GridTemplateRows.Repeat(11, px 50) ]

        let square dark king'sSquare king'sManSquare clan'sManSquare isKing'sCastle isAttemptedDragTo =
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

                  if isAttemptedDragTo then
                      BackgroundColor.aquaMarine ]

        let pieceStyle pieceType mousePos =
            fss [ Height'(pct 70)
                  Width'(pct 70)

                  match pieceType with
                  | King ->
                      BackgroundColor.orangeRed
                      Height'(pct 100)
                      Width'(pct 100)
                      BorderRadius'(pct 100)
                  | King'sMan -> BackgroundColor.orangeRed
                  | Clan'sMan -> BackgroundColor.black

                  BorderRadius'(pct 100)

                  Position.Absolute
                  Transforms [ Transform.TranslateX(pct -50)
                               Transform.TranslateY(pct -50) ]
                  Left'(pct 50)
                  Top'(pct 50)

                  match mousePos with
                  | None -> ()
                  | Some (x, y) ->
                      Left'(px x)
                      Top'(px y)

                   ]

        let serializePiecePosition piece (i, j) =
            let pieceName =
                match piece with
                | King -> "King"
                | King'sMan -> "KingMan"
                | Clan'sMan -> "ClanMan"

            sprintf "%s:%i:%i" pieceName i j

        let parsePiecePosition (piecePos: string) =
            piecePos.Split(':')
            |> function
            | [| piece; i; j |] ->
                let i = parse i
                let j = parse j

                match piece, i, j with
                | "King", Some i, Some j -> Some(King, i, j)
                | "KingMan", Some i, Some j -> Some(King'sMan, i, j)
                | "ClanMan", Some i, Some j -> Some(Clan'sMan, i, j)
                | _ -> None
            | _ -> None

        let allLegalMoves = legalMoves model

        div [ ClassName grid ]
        <| (boardCoords
            |> List.ofSeq
            |> List.map
                (fun (i, j) ->
                    div [ Key $"{i},{j}"
                          ClassName(
                              square
                                  (isDarkSquare i j)
                                  (isKing'sSquare i j)
                                  (isKing'sManSquare i j)
                                  (isClan'sManSquare i j)
                                  (isKing'sCastle (i, j))
                                  (allLegalMoves |> Set.contains (i, j))
                          )
                          OnDragOver(fun e -> e.preventDefault ())
                          OnDrop
                              (fun e ->
                                  e.preventDefault ()

                                  e.dataTransfer.getData ("dragging")
                                  |> parsePiecePosition
                                  |> Option.map (fun (piece, x, y) -> (piece, (x, y), (i, j)) |> Move |> dispatch)
                                  |> ignore) ] [
                        let (mouseX, mouseY) = model.mousePos

                        let (draggingI, draggingJ) =
                            match model.currentlyDragging with
                            | Some (piece, i, j) -> i, j
                            | None -> 0, 0

                        let drawPiece piece =
                            div [ //Key $"{}:{}"
                                  ClassName(
                                      pieceStyle
                                          piece
                                          (if model.currentlyDragging <> None
                                              && draggingI = i
                                              && draggingJ = j then
                                               Some(int mouseX, int mouseY)
                                           else
                                               None)
                                  )
                                  Draggable true
                                  OnDragStart
                                      (fun e ->
                                          dispatch <| DragStart(piece, i, j)
                                          //e.dataTransfer.setData("dragging", serializePiecePosition piece (i,j))
                                          |> ignore) ] []

                        if model.boardState.king'sPosition = (i, j) then
                            drawPiece King

                        if model.boardState.king'sMen
                           |> toSet
                           |> Set.contains (i, j) then
                            drawPiece King'sMan

                        if model.boardState.clan'sMen
                           |> toSet
                           |> Set.contains (i, j) then
                            drawPiece Clan'sMan
                    ]))

    let onMouseMove dispatch (e: Browser.Types.Event) =
        let e = e :?> MouseEvent
        MouseMove(e.clientX, e.clientY) |> dispatch

    let sub initial =
        let sub' dispatch =
            //window.onmousemove onMouseMove
            document.addEventListener ("mousemove", onMouseMove dispatch)
            ()

        Cmd.ofSub sub'


    Program.mkProgram init update render
    |> Program.withSubscription sub
    |> Program.withReactSynchronous "elmish-app"
    |> Program.run
