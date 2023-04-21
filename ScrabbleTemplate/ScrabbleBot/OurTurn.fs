namespace NoStepFunction

open Types
open ScrabbleUtil

module internal OurTurn =


    
    // let positionsAvalible : bool = 
    //     Map.fold (fun acc (element : ) -> )


    // let rec positionsAvalible2 (board : board) : bool = 
    //     match board.squares with 
    //         | Failure err -> false  
    //         | Succes -> true 


// let placeOnSquare (s: square) (st: State) : option =
//     match s.IsEmpty with
//     | false -> Failure 
//     | _ -> 

let firstMove (my : hand) (t : tiles) (d : dict) =
    //Get the first tile from your hand
    let firstTile = MultiSet.toList my |> List.head 

    //Then try to create a word out of the first word
    let wordsToPlay = Dictionary.step firstTile d

    let rec aux (word: List) (tilesLeft: List) (d : dict)= 
        match Dictionary.step (List.head tilesLeft) d with
        | Some (true, _) -> 
            word@[List.head tilesLeft] 
        | Some (false, d') -> 
            aux (word@[List.head tilesLeft]) (List.tail tilesLeft) d'
        | None (false, d') -> 
            aux List.Empty (List.tail@List.head tilesLeft) d'
            //Prøv at bruge fold til at gøre det bedre
            //aux List.Empty (List.fold (fun acc elm -> acc + elm) acc tilesLeft )
        //aux List.Empty (MultiSet.toList my) d
                
    //None så skal vi recursiv kalde step functionen på det næste elements plads.

            

        

let isACharAtCoord coord (t : tiles) = 
    match Map.tryFind coord t with
    | None _ -> (false, '?')
    | Some ch -> (true, ch)





    

