namespace NoStepFunction

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

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
        myTurn        : bool
        tiles         : Map<coord , char>
        //positionAvalible: bool
    }

    let mkState b d pn h my t= {board = b; dict = d;  playerNumber = pn; hand = h ; myTurn = my; tiles = t}
    //positionAvalible = pa
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let myTurn st = st.myTurn
    //let positionAvalible st = st.positionAvalible

module Scrabble =   
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            if(st.myTurn) 
                then 
                debugPrint("***************Now it's my turn to play**************\n") 
                Print.printHand pieces (State.hand st)
                //let testHand = MultiSet.addSingle 1u MultiSet.empty |> MultiSet.addSingle 16u |> MultiSet.addSingle 5u
                //Print.printHand pieces testHand
                //let move2 = findFirstWord testHand st.dict (0,0)
                let move2 = findFirstWord (State.hand st) st.dict
                let placeMove = placeOnBoard move2 (0,0) (1,0)
                debugPrint(sprintf "uuuuuuuuuuuuuup %A\n" placeOnBoard)

                debugPrint(sprintf "øøøøøøøøøøøøøøøøøøOurTurn found thisøøøøøøøøøøø %A\n" move2)
                forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                //let input =  System.Console.ReadLine()
                //let move = RegEx.parseMove input
                let move = placeMove
                
                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                //send cstream (SMPlay move)
                send cstream (SMPlay move)
                debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move)
            
            else debugPrint("---------------It's not my turn to play------------------\n")
            
            // remove the force print when you move on from manual input (or when you have learnt the format)
            let msg = recv cstream
           // keep the debug lines. They are useful.
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = State.mkState 
                                st.board 
                                st.dict 
                                st.playerNumber 
                                st.hand 
                                false
                                st.tiles

                debugPrint("!!!!!!!!!!I have played a succesful move!!!!!!!!!!\n") // Th
                aux st'
                
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = State.mkState 
                            st.board 
                            st.dict 
                            st.playerNumber 
                            st.hand 
                            true 
                            st.tiles // This state needs to be updated
                debugPrint("??????????They have played a succesful move????????????\n")
                aux st'
                
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                if(st.myTurn) 
                then
                    let st' = State.mkState 
                                st.board 
                                st.dict 
                                st.playerNumber 
                                st.hand 
                                true 
                                st.tiles
                    debugPrint("xxxxxxxxx You have failed a move\n")
                    aux st'

                else
                    let st' = State.mkState st.board st.dict st.playerNumber st.hand false st.tiles
                    debugPrint "xxxxxxxxxx They have failed a move\n"
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet (playerNumber = playerTurn) Map.empty)
        