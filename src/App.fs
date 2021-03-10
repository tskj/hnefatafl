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
    
    (**
    * Library helper functions
    *)

    let uncurry f (x, y) = f x y

    let parse (s: string) =
        match Int32.TryParse s with
        | (true, i) -> Some i
        | _ -> None
        
    (**
    * Domain-specific helper functions
    *)

    let toSet =
        List.collect
            (function
            | Some x -> [ x ]
            | None -> [])
        >> Set.ofList
        
    (**
    * Domain-specific operations
    *)

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

    let isKingsSquare i j = i = 0 && j = 0

    let isKingsManSquare (i: int) (j: int) =
        let i' = Math.Abs i
        let j' = Math.Abs j

        [ (0, 1)
          (0, 2)
          (1, 0)
          (2, 0)
          (1, 1) ]
        |> List.contains (i', j')

    let isClansManSquare (i: int) (j: int) =
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
    let isKingsCastle (i, j) = (i = (boardSize / 2) || i = -(boardSize / 2)) && (j = (boardSize / 2) || j = -(boardSize / 2))

    let boardCoords =
        [ -(boardSize / 2) .. (boardSize / 2) ]
        |> List.collect (fun i -> [ -(boardSize / 2) .. (boardSize / 2) ]
                                  |> List.map (fun j -> i, j))
        |> Set.ofList

    type Piece =
        | King
        | KingsMan
        | ClansMan
        
    type PiecePosition =
        int * int
        
    type ScreenPosition =
        int * int
        
    type Board =
        { kingsPosition: PiecePosition
          kingsMen: PiecePosition option list
          clansMen: PiecePosition option list }

    let initBoard () =
        { kingsPosition = 0, 0
          kingsMen =
              boardCoords
              |> List.ofSeq
              |> List.filter (uncurry isKingsManSquare)
              |> List.map Some
          clansMen =
              boardCoords
              |> List.ofSeq
              |> List.filter (uncurry isClansManSquare)
              |> List.map Some }

    type Player =
        | KingSide
        | ClanSide


    type Model =
        { boardState: Board
          winner: Player option
          playerToMove: Player
          squareSize: int
          currentlyDragging: PiecePosition option
          startDragMousePos: ScreenPosition
          animationReleaseScreenPosition: PiecePosition * ScreenPosition }

    type Msg =
        | Move of PiecePosition * PiecePosition
        | DragStart of PiecePosition * ScreenPosition
        | DragEnd of (PiecePosition * ScreenPosition) option
        | Capture of PiecePosition
        | CheckWinner

    let init () =
        { boardState = initBoard ()
          winner = None
          playerToMove = ClanSide
          squareSize = 50
          currentlyDragging = None
          startDragMousePos = 0, 0
          animationReleaseScreenPosition = (0, 0), (0, 0) },
        Cmd.none


    let tee f x =
        f x
        x
        
    let gameSpaceToScreenSpace squareSize (i,j) =
        let screenSpaceY = (boardSize / 2 + i) * squareSize
        let screenSpaceX = (boardSize / 2 + j) * squareSize
        (screenSpaceX, screenSpaceY)
        
    let getPiece (i,j) (board: Board) =
        if board.kingsPosition = (i,j) then
            Some King
        else if board.kingsMen |> List.contains (Some (i,j)) then
            Some KingsMan
        else if board.clansMen |> List.contains (Some (i,j)) then
            Some ClansMan
        else
            None

    let legalMoves (fromX, fromY) (gameState: Model) =
        let board = gameState.boardState
        let pieceToMove = board |> getPiece (fromX, fromY)

        match (fromX, fromY), pieceToMove, gameState.winner with
        | _, _, Some _ -> Set.empty
        | _, None, _ -> Set.empty
        | (x, y), Some piece, _ ->
            if (piece = King || piece = KingsMan)
               && gameState.playerToMove = ClanSide
               || piece = ClansMan
                  && gameState.playerToMove = KingSide 
            then
                Set.empty
            else
                let isKing =
                    piece = King

                let occupiedSquares =
                    board.kingsMen
                    |> toSet
                    |> Set.add board.kingsPosition
                    |> Set.union (board.clansMen |> toSet)

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

        let kingsPiece =
            board.kingsMen
            |> toSet
            |> Set.add board.kingsPosition
            |> Set.contains (i, j)

        let clansPiece =
            board.clansMen |> toSet |> Set.contains (i, j)

        if kingsPiece then
            board.clansMen
            |> toSet
            |> Set.filter (fun x -> neighbours |> Set.contains x)
        else if clansPiece then
            board.kingsMen
            |> toSet
            |> Set.add board.kingsPosition
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
                                    kingsPosition = (toX, toY) } }
                    , Cmd.batch [ Capture(toX, toY) |> Cmd.ofMsg
                                  CheckWinner |> Cmd.ofMsg ]
                | Some KingsMan ->
                    { model with
                          boardState =
                              { model.boardState with
                                    kingsMen =
                                        model.boardState.kingsMen
                                        |> replaceGuy (fromX, fromY) (Some(toX, toY)) } }
                    , Cmd.batch [ Capture(toX, toY) |> Cmd.ofMsg
                                  CheckWinner |> Cmd.ofMsg ]
                | Some ClansMan ->
                    { model with
                          boardState =
                              { model.boardState with
                                    clansMen =
                                        model.boardState.clansMen
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
                  startDragMousePos = (mouseX, mouseY) },
            Cmd.none
        | DragEnd data ->
            let stoppedDragging = 
                { model with currentlyDragging = None }
            match model.currentlyDragging, data with
            | Some (fromX, fromY), Some ((toX, toY), (mouseX, mouseY)) ->
                let dragVector = (mouseX - fst model.startDragMousePos, mouseY - snd model.startDragMousePos)
                let (startXScreen, startYScreen) = gameSpaceToScreenSpace model.squareSize (fromX, fromY)
                { stoppedDragging with animationReleaseScreenPosition = (toX,
                                                                          toY),
                                                                          (startXScreen + fst dragVector,
                                                                            startYScreen + snd dragVector ) }
                , Move ((fromX,fromY), (toX, toY)) |> Cmd.ofMsg
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
                            kingsMen =
                                model.boardState.kingsMen
                                |> removeGuys capturedNeighbours
                            clansMen =
                                model.boardState.clansMen
                                |> removeGuys capturedNeighbours 
                      } },
            Cmd.none

        | CheckWinner ->
            let isKingWinner =
                isKingsCastle model.boardState.kingsPosition

            let isClanWinner =
                enemyNeighbours model.boardState model.boardState.kingsPosition
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
        let isDragging = model.currentlyDragging
        let (startMouseX, startMouseY) = model.startDragMousePos
        let currentDraggedPieceRef = Hooks.useRef<HTMLDivElement option> None
        let squareSize = model.squareSize
        Hooks.useEffectDisposable(fun () ->
                                      match isDragging, currentDraggedPieceRef.current with
                                      | Some (i,j), Some ref ->
                                           let i = boardSize / 2 + i
                                           let j = boardSize / 2 + j
                                           (document, "mousemove")
                                           |> attachEvent (fun e ->
                                                                let e = e :?> MouseEvent
                                                                ref.setAttribute("style", $"left: {j * squareSize}px; top: {i * squareSize}px; transform: translate3d({int e.clientX - startMouseX}px, {int e.clientY - startMouseY}px, 0px) /*scale(1.2)*/")
                                                          ,fun () ->
                                                                ref.setAttribute("style", "") )
                                      | _ ->
                                          { new IDisposable with member __.Dispose() = () }
                                  , [|isDragging; currentDraggedPieceRef; startMouseX; startMouseY; squareSize; boardSize|])
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

        let square kingsSquare kingsManSquare clansManSquare isKingsCastle isAttemptedDragTo =
            fss [ Position.Relative

                  BackgroundColor' (hex "5a3300")
                  BorderRadius' (px 10)
                  Transforms [
                      Transform.Scale 0.95
                  ]

                  if kingsSquare then
                      BackgroundColor.black

                  if kingsManSquare then
                      BackgroundColor.grey

                  if clansManSquare then
                      BackgroundColor' (hex "402d54")

                  if isKingsCastle then
                      BackgroundColor.greenYellow

                  if isAttemptedDragTo then
                      BackgroundColor.aquaMarine ]

        let pieceStyle pieceType (i,j) releasePosition =
            let (x,y) = gameSpaceToScreenSpace model.squareSize (i,j)
            
            let percent percentage x = percentage * x / 100
            
            fss [ Position.Absolute
                  Display.Flex
                  JustifyContent.Center
                  AlignItems.Center
                  
                  Left' (px x)
                  Top' (px y)
                  
                  Height' (px squareSize)
                  Width' (px squareSize)
                  
                  After [
                    Display.Block
                    Content.Value("")
                    
                    if pieceType <> King then
                        BorderRadius'(pct 100)
                        if pieceType = ClansMan then
                            BorderColor' (hex "82715b")
                        else
                            BorderColor' (hex "")
                        BorderWidth' (px 2)
                        BorderStyle.Solid
                    
                    Height' (px (percent 70 squareSize))
                    Width' (px (percent 70 squareSize))

                    match pieceType with
                      | King ->
                          BackgroundColor' (hex "963511")
                          Height' (px squareSize)
                          Width' (px squareSize)
                          BorderRadius'(pct 100)
                      | KingsMan ->
                          BackgroundColor' (hex "963511")
                      | ClansMan ->
                          BackgroundColor.black
                          
                    match releasePosition with
                      | None -> ()
                      | Some (x', y') ->
                              let x'' = x' - x
                              let y'' = y' - y
                              let dropKeyframes =
                                keyframes [
                                    frame 0 [
                                      Transforms [
                                        Transform.Translate3D(px x'', px y'', px 0)
                                      ]
                                    ]
                                    frame 100 [
                                      Transforms [
                                        Transform.Translate3D(px 0, px 0, px 0)
                                      ]
                                    ]
                                ]
                              AnimationName' dropKeyframes
                              AnimationDuration' (ms 50.)
                              AnimationTimingFunction.CubicBezier(0.71,0.,1.,0.33)
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

               let (draggingI, draggingJ) =
                   model.currentlyDragging |> Option.defaultValue (-100, -100)
               
               let isDraggingThisOne = draggingI = i && draggingJ = j
               
               let ((pieceI, pieceJ), (screenX, screenY)) = model.animationReleaseScreenPosition

               let releaseScreenPosition =
                     (if
                        i = pieceI
                        && j = pieceJ
                      then
                          Some (screenX
                                , screenY)
                      else
                          None)
               
               div [ Key $"{piece}:{index}"
                     ClassName( pieceStyle piece (i,j) releaseScreenPosition )
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
                                      (isKingsSquare i j)
                                      (isKingsManSquare i j)
                                      (isClansManSquare i j)
                                      (isKingsCastle (i, j))
                                      (allLegalMoves |> Set.contains (i,j))
                              )
                              OnMouseDown
                                 (fun e ->
                                  dispatch <| DragStart ((i,j), (int e.clientX, int e.clientY)))
                              OnMouseUp
                                  (fun e -> dispatch <| DragEnd (Some ((i,j), (int e.clientX, int e.clientY))))
                             ] [] ))
            
            div [ ClassName gridSize ] 
                [draw King ( 0, Some model.boardState.kingsPosition )]
                
            div [ ClassName gridSize ] 
                (model.boardState.kingsMen
                    |> List.zip [0..100]
                    |> List.map
                        (draw KingsMan))
                                  
            div [ ClassName gridSize ] 
                (model.boardState.clansMen
                    |> List.zip [0..100]
                    |> List.map
                        (draw ClansMan))
                
            ])

    let render' (model: Model) (dispatch: Dispatch<Msg>) =
        render (model,dispatch)
           

    Program.mkProgram init update render'
    //|> Program.withSubscription sub
    |> Program.withReactSynchronous "elmish-app"
    |> Program.run
