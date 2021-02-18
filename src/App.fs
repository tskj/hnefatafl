namespace Docs

module App =
    open Elmish
    open Elmish.React
    open Fable.React
    open Fable.React.Props
    open Fss
    open System
    
    let uncurry f (x,y) = f x y
    let parse (s: string) =
        match Int32.TryParse s with
        | (true, i) -> Some i
        | _ -> None
    
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
            king'sMen = boardCoords |> List.filter (uncurry isKing'sManSquare)
            clan'sMen = boardCoords |> List.filter (uncurry isClan'sManSquare)
        }

    type PlayerToMove =
        | KingToMove
        | ClanToMove
        
    type Model = { boardState: Board
                   playerToMove: PlayerToMove
                   currentlyDragging: (Piece * int * int) option }

    type Msg =
        | Drop of (Piece * (int * int) * (int * int))
        | SetDragging of (Piece * int * int)

    let init () = { boardState = initBoard ()
                    playerToMove = ClanToMove
                    currentlyDragging = None }
    
    let tee f x =
        f x
        x
    
    let legalMoves (gameState: Model) =
        let board = gameState.boardState
        match gameState.currentlyDragging with
        | None ->
            List.empty
        | Some (piece, x, y) ->
            if (piece = King || piece = King'sMan) && gameState.playerToMove = ClanToMove
                || piece = Clan'sMan && gameState.playerToMove = KingToMove then
                List.empty
            else 
                let isKing =
                    board.king'sPosition = (x,y)
                let occupiedSquares =
                    board.king'sPosition :: board.king'sMen @ board.clan'sMen |> Set.ofList
                let allPossibleMoves =
                    boardCoords
                    
                let rec squaresBetween (x:int,y:int) (i,j) =
                    let deltaX = Math.Sign(i - x)
                    let deltaY = Math.Sign(j - y)
                    if deltaX = 0 && deltaY = 0 ||
                       deltaX > 0 && x >= i ||
                       deltaX < 0 && x <= i ||
                       deltaY > 0 && y >= j ||
                       deltaY < 0 && y <= j then
                           Set.empty
                    else
                        squaresBetween (x + deltaX, y + deltaY) (i, j)
                        |> Set.add (x + deltaX, y + deltaY)
                        
                allPossibleMoves
                |> List.filter (fun (i,j) ->
                    // Remove occupied squares
                    occupiedSquares |> Set.contains (i,j) |> not)
                |> List.filter (fun (i,j) ->
                    // Remove all that are not horizontal or vertical 
                    let deltaX = x - i
                    let deltaY = y - j
                    deltaX = 0 || deltaY = 0 )
                |> List.filter (fun (i,j) ->
                    // Remove all occluded squares between start and end
                    let travelSquares = squaresBetween (x,y) (i,j)
                    let unoccupiedTravelSquares = Set.difference travelSquares occupiedSquares
                    unoccupiedTravelSquares = travelSquares)
                |> List.filter (fun (i,j) ->
                    // Remove all the King's squares for everyone but the King
                    [(5,5);(-5,-5);(-5,5);(5,-5);(0,0)] |> List.contains (i,j) |> not
                    || isKing )
                
    let togglePlayer playerToMove =
        match playerToMove with
        | KingToMove -> ClanToMove
        | ClanToMove -> KingToMove

    let update (msg: Msg) (model: Model): Model =
        match msg with
        | Drop (piece, (fromX, fromY), (toX, toY)) ->
            let isLegalMove = legalMoves model |> List.contains (toX, toY)
            if isLegalMove then
                match piece with
                | King ->
                    { model with boardState = { model.boardState with king'sPosition = (toX, toY) }
                                 playerToMove = togglePlayer model.playerToMove 
                                 currentlyDragging = None }
                | King'sMan ->
                    { model with boardState = {
                                model.boardState with king'sMen =
                                                        (toX, toY) ::
                                                        model.boardState.king'sMen
                                                        |> List.filter (fun (i,j) -> not (fromX = i && fromY = j)) } 
                                 playerToMove = togglePlayer model.playerToMove 
                                 currentlyDragging = None }
                | Clan'sMan ->
                    { model with boardState = {
                                model.boardState with clan'sMen =
                                                        (toX, toY) ::
                                                        model.boardState.clan'sMen
                                                        |> List.filter (fun (i,j) -> not (fromX = i && fromY = j)) } 
                                 playerToMove = togglePlayer model.playerToMove 
                                 currentlyDragging = None }
            else model
        | SetDragging data ->
            { model with currentlyDragging = Some data }
        | _ -> model

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
                      BackgroundColor.aquaMarine
            ]
            
        let pieceStyle pieceType =
            fss [
                Height' (pct 70)
                Width' (pct 70)
                
                match pieceType with
                | King ->
                    BackgroundColor.orangeRed
                    Height' (pct 100)
                    Width' (pct 100)
                    BorderRadius' (pct 100)
                | King'sMan ->
                    BackgroundColor.orangeRed
                | Clan'sMan ->
                    BackgroundColor.black
                    
                BorderRadius' (pct 100)
                
                Position.Absolute
                Transforms [
                    Transform.TranslateX (pct -50)
                    Transform.TranslateY (pct -50)
                ]
                Left' (pct 50)
                Top' (pct 50)
            ]
            
        let serializePiecePosition piece (i,j) =
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
                | "King", Some i, Some j ->
                    Some (King, i, j)
                | "KingMan", Some i, Some j  ->
                    Some (King'sMan, i, j)
                | "ClanMan", Some i, Some j ->
                    Some (Clan'sMan, i, j)
                | _ -> None
            | _ ->
                None

        let allLegalMoves = legalMoves model
                        
        div [ ClassName grid ] <|
        (boardCoords |> List.map
            (fun (i,j) ->
                div [ ClassName(
                        square
                          (isDarkSquare i j)
                          (isKing'sSquare i j)
                          (isKing'sManSquare i j)
                          (isClan'sManSquare i j)
                          (isKing'sCastle i j)
                          (allLegalMoves |> List.contains (i,j)) )
                      OnDragOver (fun e -> e.preventDefault())
                      OnDrop (fun e ->
                          e.preventDefault()
                          e.dataTransfer.getData("dragging")
                          |> parsePiecePosition
                          |> Option.map (fun (piece, x, y) ->
                                                (piece, (x, y), (i, j))
                                                    |> Drop
                                                    |> dispatch
                                        ) |> ignore)
                ]
                    [
                        let drawPiece piece = 
                            div [ ClassName (pieceStyle piece)
                                  Draggable true
                                  OnDragStart (fun e ->
                                      dispatch <| SetDragging (piece, i, j) 
                                      e.dataTransfer.setData("dragging", serializePiecePosition piece (i,j))
                                      |> ignore)
                                  ] []
                            
                        if model.boardState.king'sPosition = (i, j) then
                            drawPiece King
                            
                        if model.boardState.king'sMen |> List.contains (i, j) then
                            drawPiece King'sMan
                            
                        if model.boardState.clan'sMen |> List.contains (i, j) then
                            drawPiece Clan'sMan
                    ]))

    Program.mkSimple init update render
    |> Program.withReactSynchronous "elmish-app"
    |> Program.run
