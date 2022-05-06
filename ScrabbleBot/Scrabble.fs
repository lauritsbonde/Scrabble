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

        placedTiles     : Map<coord, char>
    }

    type move = (coord * (uint32 * (char * int))) list
    // coord, (id, (letter, points)) list
    
    type newPieces = (uint32 * uint32) list
    // (id, amount) list

    let mkState b d pn pt h = {board = b; dict = d;  playerNumber = pn; placedTiles = pt; hand = h}

    let board st           = st.board
    let dict st            = st.dict
    let playerNumber st    = st.playerNumber
    let hand st            = st.hand


    let removePieces (ms : move) (st: state) : state = 
        List.fold (fun acc (_, (id, (_, _))) -> 
            MultiSet.removeSingle id acc
        ) st.hand ms |> mkState st.board st.dict st.playerNumber st.placedTiles

    let addPieces (pieces: newPieces) (st: state) = 
        List.fold (fun acc (id, amount) ->
            MultiSet.add id amount acc
        ) st.hand pieces |> mkState st.board st.dict st.playerNumber st.placedTiles

    let updateBoard (ms : move) (st: state) = 
        List.fold (fun acc (coord , (_ , (char , _))) -> 
            Map.add coord char acc
        ) st.placedTiles ms |> mkState st.board st.dict st.playerNumber st.hand

module Scrabble =
    open System.Threading

    // needs to find a move in the form of string: <x-coordinate> <y-coordinate> <piece id><character><point-value>
    (* TODO:
        check for first move - if the center point is not empty then it is the first move
        if first move - find a word from player hand
        if not first move - find a tile on the board and make a word from the playerhand that contains the tile(s)
    *)

    let rec firstMove  (*coord *) (st: State.state) (pieces: Map<uint32, tile>) customHand =
        
        // match coord board with
        // | Some c -> 
        // | None -> 

        
        let rec aux hand dict currentWord (bestword: string) = 
            debugPrint (sprintf "HANDSIZ: %A\n" (MultiSet.size hand))
            if MultiSet.isEmpty hand then 
                bestword
            else 
                MultiSet.fold (fun acc x n -> 
                    let leftovers = MultiSet.removeSingle x hand
                    Set.fold (fun acc2 (c, _) -> 
                        let word = currentWord + (string c)
                        match Dictionary.step c dict with
                        | Some (isWord, newDict) -> 
                            if isWord then
                                debugPrint (sprintf "word - %A, isword %A - char %A\n" word isWord c)
                                // debugPrint (sprintf "word - %A\n" word)
                                aux leftovers newDict word word
                            else
                                debugPrint (sprintf "word - %A, isword %A - char %A\n" word isWord c)
                                aux leftovers newDict word bestword
                        | None ->
                            bestword
                    ) "" (Map.find x pieces)
                ) "" hand
        aux customHand st.dict "" ""                

    

    let nextMove (st : State.state) pieces = 
        // check if it is the first move of the game
        let customHand = MultiSet.empty |> MultiSet.addSingle 8u |> MultiSet.addSingle 9u |> MultiSet.addSingle 20u
        let first = firstMove st pieces customHand
        debugPrint (sprintf "stringhand = %A" first)
        ""  
        

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            // let input =  System.Console.ReadLine()
            let input = nextMove st pieces
            debugPrint (sprintf "Laurits: %s \n" input)
            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                //let st' = {st.board, st.dict, st.playerNumber, st.hand, st.numberOfPlayers, st.turnCount} // This state needs to be updated
                
                // remove ms from hand
                // add newPieces to hand
                let st' = State.removePieces ms st |> State.addPieces newPieces

                // update board

                
                aux st
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //let st' = {st.board, st.dict, st.playerNumber, st.hand, st.numberOfPlayers, st.turnCount} // This state needs to be updated
                aux st
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber placedTiles handSet)