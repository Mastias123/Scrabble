namespace NoStepFunction

open Types
open ScrabbleUtil
open ScrabbleUtil.DebugPrint
open HelperFunctions

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

//my is the hand we have with the tiles,  d is the trie, coord is the coordinates on the board, dir is the upcoming directions on the board
let findFirstWord (my : hand) (d : dict) (startpositions : List<(coord*char)*(coord)>) : string =
    debugPrint(sprintf "Starting positions = %A \n" startpositions)
    let rec aux (currentHand : hand) (currentDict : dict) (currentWord : string) = //currentWord is the acc we add to
        //debugPrint(sprintf "Calling aux with currentWord = %A \n" currentWord)
        //debugPrint(sprintf "Print Hand %A \n" currentHand)
        List.fold (fun (wordSoFar : string) (c : uint32) -> 
            match Dictionary.step (uintToChar c) currentDict with 
                | Some (b, d') -> //when we get a char  
                    //debugPrint(sprintf "******Looking at  letter %A\n" c)
                    
                    //debugPrint(sprintf "Trying to remove new hand: %A\n" wordSoFar)
                    let newhand = MultiSet.removeSingle c currentHand //removes the char from hand so we can look at the next
                    //Use tile so when we call aux then give the next coordinat with an direction
                    let currentString = currentWord + string (uintToChar c) //current string is the string build so far from the acc 
                    //debugPrint(sprintf "-----------------CurrentString is %A --------- CurrentWord is %A\n" currentString currentWord)
                    let WordInBranch = aux (newhand) d' currentString //gives the new hand with the string
                    //debugPrint(sprintf "Succesfull remove new hand: %A\n" wordSoFar)
                    if(b && currentString.Length > WordInBranch.Length && currentString.Length > wordSoFar.Length) //
                    then
                        //Then return our string
                        //debugPrint( sprintf "The boolean is true, current word is %A\n" (currentString))
                        currentString 
                    elif (WordInBranch.Length > currentWord.Length) then
                        //debugPrint( sprintf "The boolean is true when word > currentword, current word is %A\n" (currentString))
                        WordInBranch
                    else 
                        //Steps up in the dictionary with a new char
                        //debugPrint( sprintf "The boolean is false,%A\n" (wordSoFar))
                        wordSoFar

                | None -> //when the char is a leaf none beneth it
                    //debugPrint( sprintf "None %A\n" wordSoFar)
                    //(wordSoFar + string (uintToChar c))
                    wordSoFar
                    //aux currentHand d (wordSoFar.Substring (wordSoFar.Length - 1))
            //Call Dict step on c
            // Match that with Some and None
                //If some call aux function with hand (with removed tile) the new dict d' ()
                    //If some is (true, 'd) if wordsSoFar
                //If none then return wordSoFar
        
        ) "" (MultiSet.toList currentHand)
    aux my d ""

   
    
    

let placeOnBoard (word : string) (coordinate: coord) ((dx,dy) : coord) : list<(int * int) * (uint32 * (char * int))> =
    let rec aux (word' : string) ((x,y): coord) acc =
        match word' with
        | "" -> acc
        | w ->
            let firstLetter = w[0]
            let pointOfFirstLetter = charToPoint (charToInteger firstLetter)
            aux word'[1..] (x+dx, y+dy) (((x,y), (charToInteger firstLetter, (firstLetter, pointOfFirstLetter)))::acc)
    aux word coordinate []

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



//Først find chars hvor overbo og underbo er tomme
//Så find alle mulige ord ud fra de chars
//Functionen skal returnere en liste alle start positioner som man kan spille et ord på
//Function below will check if the coordinates to the left and right are available on the board so we can play a word and return a list of start positions
let getStarters (tiles : tiles) : List<(coord*char)*(coord)> = 
    debugPrint(sprintf"tiles are %A\n" tiles)
    let horizontel = //fold has to return the tiles aand direction. Lis<(coord*char)*coord is the char and the direction
        Map.fold(fun acc coord c -> 
            debugPrint(sprintf "Looking at cord %A and char %A\n" coord c)
            match checkDirection coord tiles right with
            | true -> 
                debugPrint("True\n")
                ((coord, c), right)::acc
            | false -> 
                debugPrint("True\n")
                acc
        ) [] tiles
    
    let vetical =
        Map.fold(fun acc1 coord1 char1 -> 
            match checkDirection coord1 tiles down with 
            | true -> ((coord1, char1), down)::acc1
            | false -> acc1
        ) [] tiles
    horizontel@vetical

    
        


