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

let rec findFirstWord (my : hand) (d : dict) (words: List<List<char>>) (soFar : List<char>) : List<List<char>> =
    // let firstTile = MultiSet.toList my |> List.head         
    // let counter = MultiSet.fold (fun acc elm -> acc + elm) 0 (MultiSet.size my)
    // if(counter <> MultiSet.size my) 
    // then 
    //     findFirstWord (MultiSet.toList my |> List.tail) d acc 
    let rec aux allWords lst = //AllWords is all the words that is returned
        match lst with 
        | [] -> allWords 
        | i::lst -> //i means that there is a element in the list and then we matches with the step function 
            let a = 
                match Dictionary.step i d with
                | Some (b, d') -> 
                    findFirstWord (MultiSet.removeSingle i my) d' (if b then (i::soFar)::words else words) (i::soFar) //soFar is the word we are constructing 
                | None -> words   
            aux (allWords@a) lst
    aux [] (MultiSet.toList my)
    
    //Go through the hand 
    //Then call step and check and append the accumulator
    //Call recursiv on the acc.



    //Get the first tile from your hand
    

    //Then try to create a word out of the first word
    //let wordsToPlay = Dictionary.step firstTile d
    
    // let goThroughHand = MultiSet.fold (fun acc elm -> ) 
        
    //     acc hand
   
    // let rec aux acc (tilesLeft: List) (d : dict) =
        
    // let rec aux (word: List) (tilesLeft: List) (d : dict) acc = //word is a list of the word we can make, tileLeft is the rest on hand. 
    // //acc is the words we have found so far
    //     match Dictionary.step (List.head tilesLeft) d with
    //     | Some (true, _) -> 
    //         word@[List.head tilesLeft] 
    //     | Some (false, d') -> 
    //         aux (word@[List.head tilesLeft]) (List.tail tilesLeft) d' 
    //     | None when counter = List.length tilesLeft-> 
    //         word.
    //     | None when counter <> List.length tilesLeft ->
    //         aux word (List.tail tilesleft@List.head tilesLeft) d'
            //takes the rest of the elements by the List.tail and appends the head by putting it at then end
            //Prøv at bruge fold til at gøre det bedre
            //aux List.Empty (List.fold (fun acc elm -> acc + elm) acc tilesLeft )
        //aux List.Empty (MultiSet.toList my) d
                
    //None så skal vi recursiv kalde step functionen på det næste elements plads.

            

        

// let isACharAtCoord coord (t : tiles) = 
//     match Map.tryFind coord t with
//     | None _ -> (false, '?')
//     | Some ch -> (true, ch)





    

