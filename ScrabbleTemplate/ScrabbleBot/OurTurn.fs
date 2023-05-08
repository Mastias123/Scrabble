namespace NoStepFunction

open Types
open ScrabbleUtil
open ScrabbleUtil.DebugPrint
open HelperFunctions

module internal OurTurn =
let getStarters (tiles : tiles) : List<(coord*char)*(coord)> = 

    //debugPrint(sprintf"tiles are %A\n" tiles)
    let horizontel = //fold has to return the tiles aand direction. Lis<(coord*char)*coord is the char and the direction
        Map.fold(fun acc (x,y) c -> 
            match checkDirection (x,y) tiles right with
            | true ->  
                //debugPrint("horizontel FIRST TRUE\n")
                match checkRightDirection (x+1,y) tiles right with
                |true -> 
                    match checkDownDirection (x+1,y ) tiles down with
                    |true -> 
                        match checkUpRightDirection (x,y) tiles upRigt with
                        |true -> 
                        //debugPrint("horizontel SECOND TRUE\n")
                            (((x,y), c), right)::acc 
                        |false -> 
                            acc
                        //debugPrint("horizontel SECOND TRUE\n")
                    |false -> 
                        acc
                |false ->
                    //debugPrint("horizontel SECOND FALSE\n") 
                    acc  
            | false ->  
                //debugPrint("horizontel FIRST FALSE\n")
                acc 
        ) [] tiles
    // debugPrint("8888888888888888888888888888888888888888\n")
    // debugPrint(sprintf "horizontel LIST is %A\n" horizontel)
    // debugPrint("8888888888888888888888888888888888888888\n")
    //debugPrint(sprintf "\nlasthorizontel LIST is %A\n" lasthorizontel)

    //debugPrint(sprintf "Looking at cord %A and char %A\n" coord c) 
    let vetical =
        Map.fold(fun acc1 (x2,y2) char1 -> 
            match checkDirection (x2,y2) tiles down with 
            | true -> 
                //debugPrint("vetical FIRST TRUE\n")
                match checkRightDirection (x2,y2+1) tiles right with
                |true -> 
                    match checkDownDirection (x2,y2+1) tiles down with
                    |true -> 
                        //debugPrint("horizontel SECOND TRUE\n")
                         match checkDownLeftDirection (x2,y2) tiles downLeft with
                            |true -> 
                                //debugPrint("horizontel SECOND TRUE\n")
                                (((x2,y2), char1), down)::acc1
                            |false -> 
                                acc1
                    |false -> 
                        acc1
                |false ->
                    //debugPrint("horizontel SECOND FALSE\n") 
                    acc1  
            | false -> 
                //debugPrint("vetical FIRST FALSE\n")
                acc1
        ) [] tiles
    // debugPrint("8888888888888888888888888888888888888888\n")
    // debugPrint(sprintf "vetical LIST is %A\n" vetical)
    // debugPrint("8888888888888888888888888888888888888888\n")
    //debugPrint(sprintf "\nlastvetical LIST is %A\n" lastvetical)
    horizontel@vetical

let findFirstWord (my : hand) (d : dict) ((dx,dy) : coord) (startOfWord : List<coord * (uint32 * (char * int))>) (tiles : tiles) : List<coord * (uint32 * (char * int))> = 
    //debugPrint(sprintf "\n startOfWord is %A\n" startOfWord)
    let rec aux (currentHand : hand) (currentDict : dict) (currentWord : List<coord * (uint32 * (char * int))>) : List<coord * (uint32 * (char * int))>  = 

        //debugPrint("Calling aux\n")
        //debugPrint(sprintf "********currentWord is %A\n" currentWord)
        List.fold(fun (wordSoFar : List<coord * (uint32 * (char * int))>) (currentChar : uint32) ->
            let newCoord = 
                        if(currentWord.IsEmpty) 
                        then 
                        //Maybe change
                            (0,0)
                        else 
                            ((fst (fst (List.head currentWord))) + dx), ((snd (fst (List.head currentWord))) + dy)
            //debugPrint(sprintf "newCoord is %A\n" newCoord)
            if(dx > dy) //horizontal
            then 
                //debugPrint(sprintf "horizontal with %A\n " (uintToChar currentChar))
                //debugPrint(sprintf "currentChar is %A\n" (uintToChar currentChar))
                match checkDownDirection newCoord tiles down with
                    | true ->
                        match checkRightDirection newCoord tiles right with
                        | true ->
                            match checkUpDirection newCoord tiles up with
                            | true -> 
                                match Dictionary.step (uintToChar currentChar) currentDict with 
                                | None -> 
                                    //debugPrint(sprintf "None \n")
                                    //debugPrint(sprintf "------------- wordSoFar is = %A \n" wordSoFar)
                                    wordSoFar //wordSoFar
                                | Some (b, d') ->
                                    //debugPrint(sprintf "currentChar is %A\n" (uintToChar currentChar))
                                    

                                    let newhand = MultiSet.removeSingle currentChar currentHand 
                                    //debugPrint(sprintf "newhand is %A \n" newhand)
                                    
                                    
                                    let currentLetter = (newCoord, ((charToInteger (uintToChar currentChar)), ((uintToChar currentChar), (charToPoint currentChar))))
                                    //debugPrint(sprintf "newLetter is %A \n" currentLetter)
                                    let currentString = [currentLetter] @ currentWord 

                                    let longestWordInBranch = aux (newhand) d' currentString
                                    //let currentString = currentLetter::wordSoFar
                                    //let currentString = currentLetter::currentWord

                                    //let wordInBranch = aux (newhand) d' wordSoFar //gives the new hand with the string
                                    if(b && currentString.Length > longestWordInBranch.Length && currentString.Length > wordSoFar.Length) then
                                        //debugPrint(sprintf "currentString is = %A \n" currentString)
                                        currentString                    
                                    elif (longestWordInBranch.Length > currentWord.Length) then
                                        longestWordInBranch 
                                    else
                                        //debugPrint("b is false\n")
                                        wordSoFar
                            | false ->
                                wordSoFar
                        | false -> 
                            wordSoFar
                    | false ->
                        wordSoFar
            else //vertical
                //debugPrint(sprintf "vertical with %A\n " (uintToChar currentChar))
                //debugPrint(sprintf "currentChar is %A\n" (uintToChar currentChar))
                match checkDownDirection newCoord tiles down with
                    | true ->
                        match checkRightDirection newCoord tiles right with
                        | true ->
                            match checkLeftDirection newCoord tiles left with
                            | true -> 
                                match Dictionary.step (uintToChar currentChar) currentDict with 
                                | None -> 
                                    //debugPrint(sprintf "None \n")
                                    //debugPrint(sprintf "------------- wordSoFar is = %A \n" wordSoFar)
                                    wordSoFar //wordSoFar
                                | Some (b, d') ->
                                    //debugPrint(sprintf "currentChar is %A\n" (uintToChar currentChar))
                                    

                                    let newhand = MultiSet.removeSingle currentChar currentHand 
                                    //debugPrint(sprintf "newhand is %A \n" newhand)
                                    
                                    
                                    let currentLetter = (newCoord, ((charToInteger (uintToChar currentChar)), ((uintToChar currentChar), (charToPoint currentChar))))
                                    //debugPrint(sprintf "newLetter is %A \n" currentLetter)
                                    let currentString = [currentLetter] @ currentWord 

                                    let longestWordInBranch = aux (newhand) d' currentString
                                    //let currentString = currentLetter::wordSoFar
                                    //let currentString = currentLetter::currentWord

                                    //let wordInBranch = aux (newhand) d' wordSoFar //gives the new hand with the string
                                    if(b && currentString.Length > longestWordInBranch.Length && currentString.Length > wordSoFar.Length) then
                                        //debugPrint(sprintf "currentString is = %A \n" currentString)
                                        currentString                    
                                    elif (longestWordInBranch.Length > currentWord.Length) then
                                        longestWordInBranch 
                                    else
                                        //debugPrint("b is false\n")
                                        wordSoFar
                            | false ->
                                wordSoFar
                        | false -> 
                            wordSoFar
                    | false ->
                        wordSoFar
            
            
                                             
        ) [] (MultiSet.toList currentHand) 
    aux my d startOfWord

let continueWord (my : hand) (d : dict) (startPositions :  List<(coord*char)*coord>) (tiles : tiles) : List<coord * (uint32 * (char * int))> =
    //debugPrint("calling continueWord aux\n")
    let startPos =(List.rev startPositions)
    debugPrint(sprintf "\n*****************************\n")
    debugPrint(sprintf "******** Our hand is %A\n" my)
    debugPrint(sprintf "*****************************\n")
    //debugPrint(sprintf "startPos are %A\n" startPos)
    if (startPositions.IsEmpty)
    then 
        findFirstWord my d (1,0) [] tiles
    else 
        let rec aux (currentHand : hand) (currentDict : dict)  : List<coord * (uint32 * (char * int))>= 
            List.fold (fun (wordSoFar : List<coord * (uint32 * (char * int))>) ((coord,char), dir) ->
                //debugPrint(sprintf "current char is %A\n" char)
                //debugPrint("calling continueWord fold\n")
                //let startLetter = ((coord),((charToInteger (char)), ((char), (char)))) 
                match Dictionary.step char currentDict with 
                | None ->
                    wordSoFar
                | Some (_,d') -> 
                    //debugPrint(sprintf "findingFirstWord\n")
                    let startLetter : List<coord * (uint32 * (char * int))> = [(coord, ((charToInteger char),(char, (charToPoint (charToInteger char)))))]
                    //debugPrint(sprintf "\nstartLetter is %A\n" startLetter) 
                    //debugPrint(sprintf "\ncoord is %A\n" coord) 
                    let result = findFirstWord currentHand d' dir startLetter tiles
                    //debugPrint(sprintf "\nresult is %A\n\n" result) 
                    //debugPrint(sprintf "result is %A\n" result)
                    let word = result.[0..result.Length-2]
                    //debugPrint(sprintf "\nword is %A\n\n" word)
                    if (result.Length > word.Length)
                    then
                        //debugPrint("inside first if\n") 
                        word
                    elif (result.IsEmpty) then
                        //debugPrint("inside second if\n") 
                        //debugPrint(sprintf "wordSoFar is %A\n" wordSoFar) 
                        wordSoFar
                    else
                        //debugPrint("inside third second if\n") 
                        word
                    
            ) [] startPos //stepper på startPositions
        aux my d

let placeOnBoard (word : List<coord * (uint32 * (char * int))>) : List<(coord) * (uint32 * (char * int))> =
    word
 
 