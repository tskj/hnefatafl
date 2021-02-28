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

    let boardSize = 11
    let isKing'sCastle (i, j) = (i = (boardSize / 2) || i = -(boardSize / 2)) && (j = (boardSize / 2) || j = -(boardSize / 2))

    let boardCoords =
        [ -(boardSize / 2) .. (boardSize / 2) ]
        |> List.collect (fun i -> [ -(boardSize / 2) .. (boardSize / 2) ]
                                  |> List.map (fun j -> i, j))
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
          currentlyDragging: (int * int) option
          mousePos: int * int }

    type Msg =
        | Move of (int * int) * (int * int)
        | DragStart of ((int * int) * (int * int))
        | DragEnd of (int * int) option
        | Capture of int * int
        | CheckWinner

    let init () =
        { boardState = initBoard ()
          winner = None
          playerToMove = ClanSide
          currentlyDragging = None
          mousePos = 0, 0 },
        Cmd.none


    let tee f x =
        f x
        x
        
    let getPiece (i,j) (board: Board) =
        if board.king'sPosition = (i,j) then
            Some King
        else if board.king'sMen |> List.contains (Some (i,j)) then
            Some King'sMan
        else if board.clan'sMen |> List.contains (Some (i,j)) then
            Some Clan'sMan
        else
            None

    let legalMoves (fromX, fromY) (gameState: Model) =
        printf "legal moves"
        let board = gameState.boardState
        let pieceToMove = board |> getPiece (fromX, fromY)

        match (fromX, fromY), pieceToMove, gameState.winner with
        | _, _, Some _ -> Set.empty
        | _, None, _ -> Set.empty
        | (x, y), Some piece, _ ->
            if (piece = King || piece = King'sMan)
               && gameState.playerToMove = ClanSide
               || piece = Clan'sMan
                  && gameState.playerToMove = KingSide 
            then
                Set.empty
            else
                let isKing =
                    piece = King

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
        | Move ((fromX, fromY), (toX, toY)) ->
            let isLegalMove =
                legalMoves (fromX, fromY) model |> Set.contains (toX, toY)

            if isLegalMove then
                match model.boardState |> getPiece (fromX,fromY) with
                | Some King ->
                    { model with
                          boardState =
                              { model.boardState with
                                    king'sPosition = (toX, toY) } }
                    , Cmd.batch [ Capture(toX, toY) |> Cmd.ofMsg
                                  CheckWinner |> Cmd.ofMsg ]
                | Some King'sMan ->
                    { model with
                          boardState =
                              { model.boardState with
                                    king'sMen =
                                        model.boardState.king'sMen
                                        |> replaceGuy (fromX, fromY) (Some(toX, toY)) } }
                    , Cmd.batch [ Capture(toX, toY) |> Cmd.ofMsg
                                  CheckWinner |> Cmd.ofMsg ]
                | Some Clan'sMan ->
                    { model with
                          boardState =
                              { model.boardState with
                                    clan'sMen =
                                        model.boardState.clan'sMen
                                        |> replaceGuy (fromX, fromY) (Some(toX, toY)) } }
                    , Cmd.batch [ Capture(toX, toY) |> Cmd.ofMsg
                                  CheckWinner |> Cmd.ofMsg ]
                | _ ->
                    model, Cmd.none
            else
                model, Cmd.none

        | DragStart ((i,j),(mouseX,mouseY)) ->
            { model with
                  currentlyDragging = Some (i,j)
                  mousePos = (mouseX, mouseY) },
            Cmd.none
        | DragEnd pos ->
            let stoppedDragging = 
                { model with currentlyDragging = None }
            match model.currentlyDragging, pos with
            | Some (fromX, fromY), Some (toX, toY) ->
                stoppedDragging
                , (Move ((fromX,fromY), (toX, toY)) |> Cmd.ofMsg)
            | _ ->
                stoppedDragging
                , Cmd.none
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
                                |> removeGuys capturedNeighbours
                            clan'sMen =
                                model.boardState.clan'sMen
                                |> removeGuys capturedNeighbours 
                      } },
            Cmd.none

        | CheckWinner ->
            let isKingWinner =
                isKing'sCastle model.boardState.king'sPosition

            let isClanWinner =
                enemyNeighbours model.boardState model.boardState.king'sPosition
                |> Set.count = 4

            { model with
                  playerToMove = togglePlayer model.playerToMove
                  winner =
                      if isKingWinner then Some KingSide
                      else if isClanWinner then Some ClanSide
                      else None },
            Cmd.none
            
    let attachEvent (f: Event -> unit, cleanup: unit -> unit) (node: Node, eventType: string) =
        node.addEventListener(eventType, f)
        { new IDisposable with
            member __.Dispose() =
                cleanup()
                node.removeEventListener(eventType, f) }

    let render = FunctionComponent.Of(fun (model: Model, dispatch: Dispatch<Msg>) ->
        let isDragging = model.currentlyDragging <> None
        let (startMouseX, startMouseY) = model.mousePos
        let currentDraggedPieceRef = Hooks.useRef<HTMLDivElement option> None
        Hooks.useEffectDisposable(fun () ->
                                      match isDragging, currentDraggedPieceRef.current with
                                      | true, Some ref ->
                                           (document, "mousemove")
                                           |> attachEvent (fun e ->
                                                                let e = e :?> MouseEvent
                                                                ref.setAttribute("style", $"transform: translate3d({int e.clientX - startMouseX}px, {int e.clientY - startMouseY}px, 0px)")
                                                          ,fun () ->
                                                                ref.setAttribute("style", "") )
                                      | _ ->
                                          { new IDisposable with member __.Dispose() = () }
                                  , [|isDragging; currentDraggedPieceRef|])
        let squareSize = 50
        let grid =
            fss [ Display.Grid
                  Position.Absolute
                  GridTemplateColumns.Repeat(11, px squareSize)
                  GridTemplateRows.Repeat(11, px squareSize) ]
            
        let gridSize =
            fss [ Position.Absolute
                  
                  Width' (px (11 * squareSize))
                  Height' (px (11 * squareSize))
                  
                  PointerEvents.None
            ]

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

        let pieceStyle pieceType (i,j) mousePos =
            let i = boardSize / 2 + i
            let j = boardSize / 2 + j
            
            let percent percentage x = percentage * x / 100
            
            fss [ Position.Absolute
                  Display.Flex
                  JustifyContent.Center
                  AlignItems.Center
                  
                  Left' (px (j * squareSize))
                  Top' (px (i * squareSize))
                  
                  Height' (px squareSize)
                  Width' (px squareSize)
                  
                  After [
                    Display.Block
                    Content.Value("")
                    BorderRadius'(pct 100)
                    
                    Height' (px (percent 70 squareSize))
                    Width' (px (percent 70 squareSize))

                    match pieceType with
                      | King ->
                          BackgroundColor.orangeRed
                          Height' (px squareSize)
                          Width' (px squareSize)
                          BorderRadius'(pct 100)
                      | King'sMan ->
                          BackgroundColor.orangeRed
                      | Clan'sMan ->
                          BackgroundColor.black
                  ]
                  ]

        let allLegalMoves =
                match model.currentlyDragging with
                | None ->
                    Set.empty
                | Some (x,y) ->
                        legalMoves (x,y) model
        
        let draw piece =
           function
           | _, None ->
               fragment [] []
           | index, Some (i,j) ->
               let (mouseX, mouseY) = model.mousePos

               let (draggingI, draggingJ) =
                   match model.currentlyDragging with
                   | Some (i, j) -> i, j
                   | None -> 0, 0
               
               let isDraggingThisOne = 
                        model.currentlyDragging <> None
                                                && draggingI = i
                                                && draggingJ = j

               div [ Key $"{piece}:{index}"
                     ClassName(
                         pieceStyle
                             piece (i,j)
                             (if isDraggingThisOne then
                                  Some(int mouseX, int mouseY)
                              else
                                  None)
                     )
                     Ref (fun ref ->
                                if isDraggingThisOne then
                                        let ref = ref :?> HTMLDivElement
                                        currentDraggedPieceRef.current <- Some ref)
                    ] []

        fragment [] [
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
                                      (allLegalMoves |> Set.contains (i,j))
                              )
                              OnMouseDown
                                 (fun e ->
                                  dispatch <| DragStart ((i,j), (int e.clientX, int e.clientY)))
                              OnMouseUp
                                  (fun _ -> dispatch <| DragEnd (Some (i,j)))
                             ] [] ))
            
            div [ ClassName gridSize ] 
                [draw King ( 0, Some model.boardState.king'sPosition )]
                
            div [ ClassName gridSize ] 
                (model.boardState.king'sMen
                    |> List.zip [0..100]
                    |> List.map
                        (draw King'sMan))
                                  
            div [ ClassName gridSize ] 
                (model.boardState.clan'sMen
                    |> List.zip [0..100]
                    |> List.map
                        (draw Clan'sMan))
                
            ])

    let render' (model: Model) (dispatch: Dispatch<Msg>) =
        render (model,dispatch)
           

    Program.mkProgram init update render'
    //|> Program.withSubscription sub
    |> Program.withReactSynchronous "elmish-app"
    |> Program.run
