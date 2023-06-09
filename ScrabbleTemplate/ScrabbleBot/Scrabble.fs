namespace NoStepFunction

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open MultiSet

open OurTurn

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

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        numOfPlayers  : uint32
        playerTurn    : uint32
        tiles         : Map<coord , char>
        //positionAvalible: bool
    }

    let mkState b d pn h np pt t= {board = b; dict = d;  playerNumber = pn; hand = h ; numOfPlayers = np ; playerTurn = pt; tiles = t}
    //positionAvalible = pa
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let numOfPlayers st  = st.numOfPlayers
    let playerTurn st    = st.playerTurn
    //let positionAvalible st = st.positionAvalible

    

module Scrabble =   
    open System.Threading

    let updateTiles ms tiles = 
        List.fold (fun acc (coord, (_, (char, _))) -> Map.add coord char acc) tiles ms
    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            match st.playerNumber with
                |_ when st.playerTurn = st.playerNumber ->
                    debugPrint("***************Now it's my turn to play**************\n") 
                    //Print.printHand pieces (State.hand st)
                    //let testHand = MultiSet.addSingle 1u MultiSet.empty |> MultiSet.addSingle 16u |> MultiSet.addSingle 5u
                    //Print.printHand pieces testHand
                    //let move2 = findFirstWord testHand st.dict (0,0) (0,1) (getStarters st.tiles)
                    //debugPrint(sprintf "øøøøøøøøøøøøøøøøøøOurTurn found this    %A\n" move2)
                    //debugPrint(sprintf "calling getStarters wiht %A\n" (getStarters st.tiles))
                    debugPrint(sprintf "--------------------------------------------\n")
                    debugPrint (sprintf "ST.TILES IS %A\n" st.tiles)
                    debugPrint(sprintf "--------------------------------------------\n\n")
                    debugPrint(sprintf "ooooooooooooooooooooooooooooooooooooooooooooo\n")
                    debugPrint (sprintf "GETSTARTERS IS %A\n" (getStarters st.tiles))
                    debugPrint(sprintf "ooooooooooooooooooooooooooooooooooooooooooooo\n")

                    let move2 = continueWord st.hand st.dict (getStarters st.tiles) st.tiles
                    debugPrint (sprintf "***output from findFistWord call***  %A\n" move2)
                    //let position = (getRandomStartPosition st.tiles)
                    let placeMove = placeOnBoard move2 
                    debugPrint(sprintf "placeMove is %A\n" placeMove)

                    
                    forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                    //let input =  System.Console.ReadLine()
                    //let move = RegEx.parseMove input
                    let move = placeMove
                    
                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    //send cstream (SMPlay move)
                    

                    // if (List.length placeMove = 0 && MultiSet.size st.hand = 7u) then 
                    //     debugPrint("################## CHANGING HAND ##################\n")
                    //     send cstream (SMChange (MultiSet.toList st.hand))

                    if(List.length placeMove = 0 && MultiSet.size st.hand < 7u) then
                        debugPrint(sprintf "################## PASSING TURN  PLAYER %A CANNOT PLACE WORDS ##################\n" st.playerNumber)
                        send cstream (SMPass)
                        //send cstream (SMPlay move)
                        
                    else
                        send cstream (SMPlay move)
                    

                    debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move)
                | _ -> 
                    debugPrint("----------------------Its not my my turn to play----------------------\n") 
            
              
            let switchPlayer = (st.playerTurn)%(st.numOfPlayers) + (uint32) 1
            let msg = recv cstream
            
            // keep the debug lines. They are useful.
            match msg with
            | RCM (CMPlaySuccess(piecesPlaced, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                // remove piecesPlaced from hand
                // add new pieces to hand (id of piece, and number of pieces)
                // update tiles

                //handWithOutRemovedElementsThatWerePlayedLastTurn it is our new state
                let removeFromHand = 
                    List.fold (fun acc (_, (id, (_, _)))  -> MultiSet.removeSingle id acc) st.hand piecesPlaced //remove id tile
                debugPrint(sprintf "removeFromHand %A\n" removeFromHand)
                
                let updateHand = 
                    List.fold (fun acc (c, _) -> MultiSet.addSingle c acc) removeFromHand newPieces //should add the new hand
                debugPrint(sprintf "updatedHand %A\n" updateHand)
                
                let newTiles = updateTiles piecesPlaced st.tiles
                debugPrint(sprintf "newTiles %A\n" newTiles)
                //får moves, får tiles
                debugPrint(sprintf "calling getStarters wiht %A\n" (getStarters newTiles))
           
                let st' = State.mkState 
                                st.board 
                                st.dict 
                                st.playerNumber 
                                updateHand 
                                st.numOfPlayers
                                switchPlayer
                                newTiles

                debugPrint("!!!!!!!!!!I have played a succesful move!!!!!!!!!!\n") // Th
                aux st'
                    
                
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                // update tiles
                
                let newTiles = updateTiles ms st.tiles
                let st' = State.mkState 
                            st.board 
                            st.dict 
                            st.playerNumber 
                            st.hand 
                            st.numOfPlayers
                            switchPlayer
                            newTiles // This state needs to be updated
                debugPrint("??????????They have played a succesful move????????????\n")
                aux st'
                
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)

                let newTiles = updateTiles ms st.tiles
                match st.playerNumber with
                    |_ when st.playerTurn = st.playerNumber ->
                        let st' = State.mkState 
                                    st.board 
                                    st.dict 
                                    st.playerNumber 
                                    st.hand 
                                    st.numOfPlayers
                                    switchPlayer 
                                    newTiles
                        debugPrint("xxxxxxxxx You have failed a move\n")
                        aux st'

                    |_ ->
                        let st' = State.mkState 
                                    st.board 
                                    st.dict 
                                    st.playerNumber 
                                    st.hand 
                                    st.numOfPlayers
                                    switchPlayer 
                                    st.tiles
                        debugPrint "xxxxxxxxxx They have failed a move\n"
                        aux st'
            | RCM (CMForfeit _) ->
                debugPrint(sprintf "Player %A has forfeit\n" st.playerNumber)
                let newNumberOfPlayers = st.numOfPlayers - 1u
                let st' = State.mkState 
                                    st.board 
                                    st.dict 
                                    st.playerNumber 
                                    st.hand 
                                    newNumberOfPlayers
                                    switchPlayer 
                                    st.tiles
                aux st'
            
            | RCM (CMGameOver _) -> ()

            | RCM a  -> 
                let st' = State.mkState 
                                    st.board 
                                    st.dict 
                                    st.playerNumber 
                                    st.hand 
                                    st.numOfPlayers
                                    switchPlayer 
                                    st.tiles
                aux st'
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

        let numOfPlayers = numPlayers

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet numOfPlayers playerTurn Map.empty)
        