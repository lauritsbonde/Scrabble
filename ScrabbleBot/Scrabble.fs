namespace AlphaScrabZero

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    open StateMonad
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    open ScrabbleUtil

    type state = {
        board           : Parser.board
        dict            : ScrabbleUtil.Dictionary.Dict
        playerNumber    : uint32
        hand            : MultiSet.MultiSet<uint32>

        playerTurn      : uint32
        numPlayers      : uint32 // total amount of players in the game

        placedTiles     : Map<coord, char>
    }

    type move = (coord * (uint32 * (char * int))) list
    type dir = Right | Down
    let next (x, y) = function
        | Right -> (x+1, y)
        | Down -> (x, y+1)
    // coord, (id, (letter, points)) list
    
    type newPieces = (uint32 * uint32) list
    // (id, amount) list

    let mkState b d pn pt h plt np = {board = b; dict = d;  playerNumber = pn; placedTiles = pt; hand = h; playerTurn = plt; numPlayers = np}

    let board st           = st.board
    let dict st            = st.dict
    let playerNumber st    = st.playerNumber
    let hand st            = st.hand


    let removePieces (ms : move) (st: state) : state = 
        List.fold (fun acc (_, (id, (_, _))) -> 
            MultiSet.removeSingle id acc
        ) st.hand ms |> mkState st.board st.dict st.playerNumber st.placedTiles <| st.playerTurn <| st.numPlayers

    let addPieces (pieces: newPieces) (st: state) = 
        List.fold (fun acc (id, amount) ->
            MultiSet.add id amount acc
        ) st.hand pieces |> mkState st.board st.dict st.playerNumber st.placedTiles <| st.playerTurn <| st.numPlayers

    let updateBoard (ms : move) (st: state) = 
        List.fold (fun acc (coord , (_ , (char , _))) -> 
            Map.add coord char acc
        ) st.placedTiles ms |> mkState st.board st.dict st.playerNumber <| st.hand <| st.playerTurn <| st.numPlayers

    let updatePlayerTurn (st: state) = 
         mkState st.board st.dict st.playerNumber st.placedTiles st.hand ((st.playerTurn % st.numPlayers) + 1u) st.numPlayers

    let isCoordOnBoard st (x,y) =
        //TODO figure out how to get the boardsize and use that instead of 7
        let boardSize = 7
        if x <= boardSize && x >= -boardSize && y >= -boardSize && y <= boardSize then
            true
        else
            false
        
    
module Scrabble =
    open System.Threading

    // needs to find a move in the form of string: <x-coordinate> <y-coordinate> <piece id><character><point-value>
    (* TODO:
        check for first move - if the center point is not empty then it is the first move
        if first move - find a word from player hand
        if not first move - find a tile on the board and make a word from the playerhand that contains the tile(s)
    *)
    let findLongestWord (possibleMoves: List<State.move>) =
        List.fold (fun acc move -> 
            match List.length move > List.length acc with
            | true  -> move
            | false -> acc
        ) List.empty possibleMoves

    let checkNoSurroundingTiles (x,y) dir (st: State.state) hasStarted =
        if State.isCoordOnBoard st (x,y) then
            if dir = State.Right then
                match Map.tryFind (x, y+1) st.placedTiles with
                    | Some _ -> false
                    | None -> 
                        match Map.tryFind (x, y-1) st.placedTiles with
                        | Some _ -> false
                        | None -> 
                            if hasStarted then
                                match Map.tryFind (x+1, y) st.placedTiles with
                                | Some _ -> false
                                | None   -> true
                            else 
                                true
            else
                match Map.tryFind (x+1, y) st.placedTiles with
                | Some _ -> false
                | None -> 
                    match Map.tryFind (x-1, y) st.placedTiles with
                    | Some _ -> false
                    | None -> 
                        if hasStarted then
                            match Map.tryFind (x, y+1) st.placedTiles with
                            | Some _ -> false
                            | None   -> true
                        else
                            true
        else 
            false

    let checkValidStartField (x,y) (st: State.state) dir = 
        if State.isCoordOnBoard st (x,y) then
            if dir = State.Right then
                    match Map.tryFind (x-1, y) st.placedTiles with
                    | Some c -> false
                    | None   -> true
                                
            else if dir = State.Down then
                match Map.tryFind (x, y-1) st.placedTiles with
                    | Some c -> false
                    | None   -> true

            else 
                false
        else 
            false

    let findMove (coords: List<ScrabbleUtil.coord>) (st: State.state) (pieces: Map<uint32, tile>) =
        debugPrint "MOVING \n"

        let rec move coord dir dict currentHand currentMove possibleMoves hasStarted startCoord =
            // find the next coordinate
            let nextCoord = State.next coord dir

            // figure out if the current coord is the startcoord and set hasStarted - this should only happen when there is a tile on the map
            let newHasStarted = 
                if hasStarted then
                    true
                else
                    coord = startCoord
            
            // check if the next coordinate has a character
            match Map.tryFind coord st.placedTiles with
            | Some c -> 
                // dictionary step with the character
                    match Dictionary.step c dict with
                    | Some (isWord, newDict) ->  
                        // if it is a word and we have started to build our move and the startcoord is a part of the move and there are no adjecent coords that does not make a valid word
                        if isWord && List.length currentMove > 0 && newHasStarted && checkNoSurroundingTiles coord dir st newHasStarted then // we need to check that when a move starts above or left of start the rest of the word is also word
                            let newPossibleMoves = currentMove :: possibleMoves
                            move nextCoord dir newDict currentHand currentMove newPossibleMoves newHasStarted startCoord
                        else 
                            move nextCoord dir newDict currentHand currentMove possibleMoves newHasStarted startCoord
                    | None -> possibleMoves // if the dictionary step fails, we can't build a word

            | None -> aux coord dir dict currentHand currentMove possibleMoves hasStarted startCoord // if the coordinate is empty, we use our hand

        // trying to add a tile from the hand to the move
        and aux coord dir dict currentHand currentMove possibleMoves hasStarted startCoord =
            // finding the next coordinate
            let newCoord = State.next coord dir

            // check the tiles in the opposite direction - if there are some drop the field
            if checkNoSurroundingTiles coord dir st hasStarted then
                // go through all tiles on the hand
                MultiSet.fold (fun acc id _ -> 
                    // create a multiset of the remaining pieces in the hand to use for next move step
                    let newHand = MultiSet.removeSingle id currentHand
                    acc @ Set.fold (fun acc2 (c, p) -> 
                        // build up the move
                        let newMove = currentMove @ [coord, ((id), (c, p))]

                        // check if we can dictionary step with the tile
                        match Dictionary.step c dict with
                        | Some (isWord, newDict) ->
                            if isWord && hasStarted then
                                let newPossibleMoves = newMove :: possibleMoves
                                acc2 @ move newCoord dir newDict newHand newMove newPossibleMoves hasStarted startCoord
                            else
                                acc2 @ move newCoord dir newDict newHand newMove possibleMoves hasStarted startCoord
                        | None -> acc2
                    ) List.empty (Map.find id pieces)
                ) possibleMoves currentHand
            else 
                possibleMoves

        let rec startAboveCoord (xstart,ystart) (xcurrent, ycurrent) moves =
            if checkValidStartField (xcurrent, ycurrent) st State.Down then
                let hasStarted = xstart = xcurrent && ystart = ycurrent
                let newMoves = moves @ move (xcurrent, ycurrent) State.Down st.dict  st.hand List.Empty List.Empty hasStarted (xstart, ystart)
                if ycurrent >= (ystart - 7) then // TODO: replace hardcoded value
                    startAboveCoord (xstart, ystart) (xcurrent, ycurrent - 1) newMoves
                else
                    moves
            else
                moves
                
        let rec startLeftOfCoord (xstart,ystart) (xcurrent, ycurrent) moves =
            if checkValidStartField (xcurrent, ycurrent) st State.Right then
                let hasStarted = xstart = xcurrent && ystart = ycurrent
                let newMoves = moves @ move (xcurrent, ycurrent) State.Right st.dict  st.hand List.Empty List.Empty hasStarted (xstart, ystart)
                if xcurrent >= (xstart - 7) then // TODO: replace hardcoded value
                    startLeftOfCoord (xstart, ystart) (xcurrent - 1, ycurrent) newMoves
                else
                    moves
            else
                moves
        

        let startDownFromCoord coord =  
            if checkValidStartField coord st State.Down then
                move coord State.Down st.dict st.hand List.empty List.empty true coord
            else 
                []
        
        let startRightFromCoord coord = 
            // debugPrint "going right \n"
            if checkValidStartField coord st State.Right then
                move coord State.Right st.dict st.hand List.empty List.empty true coord
            else 
                []

        findLongestWord (List.fold (fun acc coord -> 
                // debugPrint (sprintf "Accumalator: %A - coord: %A \n" acc coord)
                let above = startAboveCoord coord coord List.empty 
                let left = startLeftOfCoord coord coord List.empty 
                let down = startDownFromCoord coord 
                let right = startRightFromCoord coord

                // debugPrint (sprintf "above: %A\n" above)
                // debugPrint (sprintf "left: %A\n" left)
                // debugPrint (sprintf "down: %A\n" down)
                // debugPrint (sprintf "right: %A\n" right)

                acc @ left @ above @ down @ right
            ) List.Empty coords)

    
    let stringMove move =
        List.fold(fun acc ((x, y), (id, (char, point))) -> 
            $"{acc} {x} {y} {id}{char}{point}"
        ) "" move

    let nextMove (st : State.state) pieces = 
        // check if it is the first move of the game
        //let customHand = MultiSet.empty |> MultiSet.addSingle 8u |> MultiSet.addSingle 9u |> MultiSet.addSingle 20u
        if (Map.isEmpty st.placedTiles) then
            findMove [st.board.center] st pieces
        else
            let coordList = st.placedTiles |> Map.toList |> List.map fst
            findMove coordList st pieces
        

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            debugPrint (sprintf "playerturn: %A\n" st.playerTurn )
            if st.playerTurn = st.playerNumber then 
                Print.printHand pieces (State.hand st)

                // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                // let input =  System.Console.ReadLine()
                let input = nextMove st pieces
                // Check if we have found a word
                if List.isEmpty input then
                    debugPrint "VI PASSER \n"
                    send cstream (SMPass)
                else 
                    debugPrint (sprintf " VORESMOVE: %s \n" (stringMove input))
                    let move = RegEx.parseMove (stringMove input)

                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) 
                    send cstream (SMPlay move)
                    debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move)
            
            let msg = recv cstream

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                //let st' = {st.board, st.dict, st.playerNumber, st.hand, st.numberOfPlayers, st.turnCount} // This state needs to be updated
                
                // remove ms from hand
                // add newPieces to hand
                // update board
                debugPrint (sprintf "Succesfully played: %A\n" ms)
                let st' = State.removePieces ms st |> State.addPieces newPieces |> State.updateBoard ms |> State.updatePlayerTurn
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = State.updateBoard ms st |> State.updatePlayerTurn
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st |> State.updatePlayerTurn
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM (CMPassed (_)) ->
                let st' = State.updatePlayerTurn st
                aux st'
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st
            


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        let (placedTiles: Map<ScrabbleUtil.coord, char>) = Map.empty

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber placedTiles handSet playerTurn numPlayers)