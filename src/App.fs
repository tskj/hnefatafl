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
        
    let tee f x =
        f x
        x
        
        
    (**
    * Domain types
    *)

    type Piece =
        | King
        | KingsMan
        | ClansMan

    type PiecePosition =
        PiecePosition of int * int
        
    type ScreenPosition =
        ScreenPosition of int * int

    type Board =
        { kingsPosition: PiecePosition
          kingsMen: PiecePosition option list
          clansMen: PiecePosition option list }

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

    (**
    * Constants
    *)

    let boardSize = 11
    
    let boardCoords =
        [ -(boardSize / 2) .. (boardSize / 2) ]
        |> List.collect (fun i -> [ -(boardSize / 2) .. (boardSize / 2) ]
                                  |> List.map (fun j -> (i, j) |> PiecePosition))
        |> Set.ofList

    (**
    * Domain-specific operations
    *)
    
    type PieceDirection =
        PieceDirection of int * int
    
    let (<+>) (PiecePosition (i,j)) (PieceDirection (di,dj)) =
        PiecePosition (i + di, j + dj)
        
    let (<->) (PiecePosition (i,j)) (PiecePosition (i2,j2)) =
        PieceDirection (i - i2, j - j2)
        
    let (<*>) f (PieceDirection (i,j)) =
        PieceDirection (f i, f j)
    
    let yDir = PieceDirection (0, 1)
    let xDir = PieceDirection (1, 0)
    
    let neg = (*) -1

    let toSet =
        List.collect
            (function
            | Some x -> [ x ]
            | None -> [])
        >> Set.ofList
 
    let replaceGuy pos guy guys =
        guys
        |> List.map
            (function
            | Some oldGuy when oldGuy = pos -> guy
            | x -> x)

    let removeGuys guys fromGuys =
        fromGuys
        |> List.map
            (function
            | Some pos when (guys |> Set.contains pos) -> None
            | x -> x)

    let isKingsThrone =
        (=) (PiecePosition (0,0))

    let isKingsManSquare (PiecePosition (i,j)) =
        let i' = Math.Abs i
        let j' = Math.Abs j

        [ (0, 1)
          (0, 2)
          (1, 0)
          (2, 0)
          (1, 1) ]
        |> List.contains (i', j')

    let isClansManSquare (PiecePosition (i,j)) =
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
        
    let isKingsCastle (PiecePosition (i, j)) =
           (i = (boardSize / 2) || i = -(boardSize / 2))
        && (j = (boardSize / 2) || j = -(boardSize / 2))
    
    let isKingsSquare pos = isKingsThrone pos || isKingsCastle pos
        
    let gameSpaceToScreenSpace squareSize (PiecePosition (i,j)) =
        let screenSpaceY = (boardSize / 2 + i) * squareSize
        let screenSpaceX = (boardSize / 2 + j) * squareSize
        (screenSpaceX, screenSpaceY) |> ScreenPosition 

    let getPiece pos (board: Board) =
        if board.kingsPosition = pos then
            Some King
        else if board.kingsMen |> List.contains (Some pos) then
            Some KingsMan
        else if board.clansMen |> List.contains (Some pos) then
            Some ClansMan
        else
            None

    let legalMoves fromPos (gameState: Model) =
        let board = gameState.boardState
        let pieceToMove = board |> getPiece fromPos

        match fromPos, pieceToMove, gameState.winner with
        | _, _, Some _ -> Set.empty
        | _, None, _ -> Set.empty
        | PiecePosition (x, y), Some piece, _ ->
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

                let rec squaresBetween (PiecePosition (x, y)) (PiecePosition (i, j)) =
                    let deltaX = Math.Sign(i - x)
                    let deltaY = Math.Sign(j - y)

                    if deltaX = 0 && deltaY = 0
                       || deltaX > 0 && x >= i
                       || deltaX < 0 && x <= i
                       || deltaY > 0 && y >= j
                       || deltaY < 0 && y <= j then
                        Set.empty
                    else
                        squaresBetween (PiecePosition (x + deltaX, y + deltaY)) (PiecePosition (i, j))
                        |> Set.add (PiecePosition (x + deltaX, y + deltaY))

                allPossibleMoves
                |> Set.filter
                    (fun pos ->
                        // Remove occupied squares
                        occupiedSquares |> Set.contains pos |> not)
                |> Set.filter
                    (fun (PiecePosition (i,j)) ->
                        // Remove all that are not horizontal or vertical
                        let deltaX = x - i
                        let deltaY = y - j
                        deltaX = 0 || deltaY = 0)
                |> Set.filter
                    (fun pos ->
                        // Remove all occluded squares between start and end
                        let travelSquares = squaresBetween (PiecePosition (x, y)) pos

                        let unoccupiedTravelSquares =
                            Set.difference travelSquares occupiedSquares

                        unoccupiedTravelSquares = travelSquares)
                |> Set.filter
                    (fun pos ->
                        // Remove all the King's squares for everyone but the King
                        boardCoords
                        |> Set.filter isKingsSquare
                        |> Set.contains pos
                        |> not
                        || isKing)


    let enemyNeighbours (board: Board) pos =
        let neighbours =
            [ pos <+> xDir
              pos <+> (neg <*> xDir)
              pos <+> yDir
              pos <+> (neg <*> yDir) ]
            |> Set.ofList

        let kingsPiece =
            board.kingsMen
            |> toSet
            |> Set.add board.kingsPosition
            |> Set.contains pos

        let clansPiece =
            board.clansMen |> toSet |> Set.contains pos

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

    let capture (board: Board) pos =
        let horizontalAttackVector = [ pos <+> xDir; pos <+> (neg <*> xDir) ] |> Set.ofList
        let verticalAttackVector = [ pos <+> yDir; pos <+> (neg <*> yDir) ] |> Set.ofList

        let enemyNeighbours' = enemyNeighbours board pos

        let horizontalAttack =
            (horizontalAttackVector |> Set.isSubset) enemyNeighbours'

        let verticalAttack =
            (verticalAttackVector |> Set.isSubset) enemyNeighbours'

        horizontalAttack || verticalAttack

    let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
        match msg with
        | Move (fromPos, toPos) ->
            let isLegalMove =
                legalMoves fromPos model |> Set.contains toPos

            if isLegalMove then
                match model.boardState |> getPiece fromPos with
                | Some King ->
                    { model with
                          boardState =
                              { model.boardState with
                                    kingsPosition = toPos } }
                    , Cmd.batch [ Capture toPos |> Cmd.ofMsg
                                  CheckWinner |> Cmd.ofMsg ]
                | Some KingsMan ->
                    { model with
                          boardState =
                              { model.boardState with
                                    kingsMen =
                                        model.boardState.kingsMen
                                        |> replaceGuy fromPos (Some toPos) } }
                    , Cmd.batch [ Capture toPos |> Cmd.ofMsg
                                  CheckWinner |> Cmd.ofMsg ]
                | Some ClansMan ->
                    { model with
                          boardState =
                              { model.boardState with
                                    clansMen =
                                        model.boardState.clansMen
                                        |> replaceGuy fromPos (Some toPos) } }
                    , Cmd.batch [ Capture toPos |> Cmd.ofMsg
                                  CheckWinner |> Cmd.ofMsg ]
                | _ ->
                    model, Cmd.none
            else
                model, Cmd.none

        | DragStart (dragPiece, ScreenPosition (mouseX, mouseY)) ->
            { model with
                  currentlyDragging = Some dragPiece
                  startDragMousePos = (mouseX, mouseY) |> ScreenPosition },
            Cmd.none
        | DragEnd data ->
            let stoppedDragging = 
                { model with currentlyDragging = None }
            match model.currentlyDragging, data with
            | Some fromPos, Some (toPos, ScreenPosition (mouseX, mouseY)) ->
                let (ScreenPosition (startX, startY)) = model.startDragMousePos
                let dragVector = (mouseX - startX, mouseY - startY)
                let (ScreenPosition (startXScreen, startYScreen)) = gameSpaceToScreenSpace model.squareSize fromPos
                { stoppedDragging with animationReleaseScreenPosition = toPos,
                                                                          ScreenPosition
                                                                            (startXScreen + fst dragVector,
                                                                             startYScreen + snd dragVector ) }
                , Move (fromPos, toPos) |> Cmd.ofMsg
            | _ ->
                stoppedDragging
                , Cmd.none
        | Capture capturePos ->
            let enemyNeighbours' = enemyNeighbours model.boardState capturePos

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
        let (ScreenPosition (startMouseX, startMouseY)) = model.startDragMousePos
        let currentDraggedPieceRef = Hooks.useRef<HTMLDivElement option> None
        let squareSize = model.squareSize
        Hooks.useEffectDisposable(fun () ->
                                      match isDragging, currentDraggedPieceRef.current with
                                      | Some (PiecePosition (i,j)), Some ref ->
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

        let pieceStyle pieceType pos releasePosition =
            let (ScreenPosition (x,y)) = gameSpaceToScreenSpace model.squareSize pos
            
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
                              AnimationDuration' (ms 30.)
                              AnimationTimingFunction.CubicBezier(0.71,0.,1.,0.33)
                  ]
            ]

        let allLegalMoves =
                match model.currentlyDragging with
                | None -> Set.empty
                | Some pos -> legalMoves pos model
        
        let draw piece =
           function
           | _, None ->
               fragment [] []
           | index, Some (PiecePosition (i,j)) ->

               // TODO: Make not crap
               let (PiecePosition (draggingI, draggingJ)) =
                   model.currentlyDragging |> Option.defaultValue (PiecePosition (-100, -100))
               
               let isDraggingThisOne = draggingI = i && draggingJ = j
               
               let (PiecePosition (pieceI, pieceJ), ScreenPosition (screenX, screenY)) = model.animationReleaseScreenPosition

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
                     ClassName( pieceStyle piece (PiecePosition (i,j)) releaseScreenPosition )
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
                    (fun pos ->
                        let (PiecePosition (i,j)) = pos
                        div [ Key $"{i},{j}"
                              ClassName(
                                  square
                                      (isKingsThrone pos)
                                      (isKingsManSquare pos)
                                      (isClansManSquare pos)
                                      (isKingsCastle pos)
                                      (allLegalMoves |> Set.contains pos)
                              )
                              OnMouseDown
                                 (fun e ->
                                  dispatch <| DragStart (pos, (int e.clientX, int e.clientY) |> ScreenPosition))
                              OnMouseUp
                                  (fun e -> dispatch <| DragEnd (Some (pos, (int e.clientX, int e.clientY) |> ScreenPosition)))
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

    let initBoard () =
        { kingsPosition = (0, 0) |> PiecePosition
          kingsMen =
              boardCoords
              |> List.ofSeq
              |> List.filter isKingsManSquare
              |> List.map Some
          clansMen =
              boardCoords
              |> List.ofSeq
              |> List.filter isClansManSquare
              |> List.map Some }
 
    let init () =
        { boardState = initBoard ()
          winner = None
          playerToMove = ClanSide
          squareSize = 50
          currentlyDragging = None
          startDragMousePos = (0, 0) |> ScreenPosition
          animationReleaseScreenPosition = (0, 0) |> PiecePosition, (0, 0) |> ScreenPosition },
        Cmd.none

    let render' (model: Model) (dispatch: Dispatch<Msg>) =
        render (model,dispatch)
           

    Program.mkProgram init update render'
    //|> Program.withSubscription sub
    |> Program.withReactSynchronous "elmish-app"
    |> Program.run
