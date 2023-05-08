namespace NoStepFunction

open Types
open ScrabbleUtil
open ScrabbleUtil.DebugPrint
open HelperFunctions

module internal OurTurn =

//Først find chars hvor overbo og underbo er tomme
//Så find alle mulige ord ud fra de chars
//Functionen skal returnere en liste alle start positioner som man kan spille et ord på
//Function below will check if the coordinates to the left and right are available on the board so we can play a word and return a list of start positions
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
                        //debugPrint("horizontel SECOND TRUE\n")
                        (((x,y), c), right)::acc 
                    |false -> 
                        acc
                |false ->
                    //debugPrint("horizontel SECOND FALSE\n") 
                    acc  
            | false ->  
                //debugPrint("horizontel FIRST FALSE\n")
                acc 
        ) [] tiles
    //debugPrint(sprintf "\nhorizontel LIST is %A\n" horizontel)
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
                         (((x2,y2), char1), down)::acc1
                    |false -> 
                        acc1
                |false ->
                    //debugPrint("horizontel SECOND FALSE\n") 
                    acc1  
            | false -> 
                //debugPrint("vetical FIRST FALSE\n")
                acc1
        ) [] tiles
    //debugPrint(sprintf "\nvetical LIST is %A\n\n" vetical)
    //debugPrint(sprintf "\nlastvetical LIST is %A\n" lastvetical)
    horizontel@vetical


let getRandomStartPosition tiles : coord*coord=
    let starters = getStarters tiles
    if (starters.IsEmpty)
    then 
        let defaultPosition = coord(0,0), coord(1,0)
        defaultPosition 
    else
        let newStarters = 
            List.fold (fun acc ((coord, _), dir) -> (coord,dir)::acc) [] starters  
        let randomNumber = System.Random()
        let newStartPosAndDir = newStarters.[randomNumber.Next(0, newStarters.Length)]
        //debugPrint(sprintf"newStartPos = %A\n" newStartPosAndDir)
        newStartPosAndDir
let getRandomStartLetter list =
        let rnd = System.Random()
        let index = rnd.Next(0, List.length list)
        List.item index list
//Vi kan få fat på startPositions hvor man skal lave et ord fra. 
//Vi kan lægge et ord første gang
//Men vi har problemer med at starte med at steppe ud fra en af startPositionerne efter et ord er blevet lagt.
//Lige nu stepperen først med hele listen af startPositioner, men den skal kun gøre det udfra et af elementerne fra startPositionerne så vi kan bygge ordet.


// let startLetters (startPositions :List<(coord*char)*(coord)>) =
//         List.fold (fun acc ((tempCoord, tempChar), _) -> 
//             match Dictionary.step tempChar d with 
//                 | None -> acc
//                 | Some (b, _) -> 
//                     if(b)
//                     then
//                         acc
//                     else 
//                         let pointOfFirstLetter = charToPoint (charToInteger tempChar)
//                         let newStartLetter = (tempCoord,((charToInteger tempChar), (tempChar, pointOfFirstLetter)))::acc
//                         debugPrint(sprintf "newStartLetter is %A \n" newStartLetter)
//                         newStartLetter                       
//     ) [] startPositions
    
    //let startElement = List.head startLetters  
    
    //Skulle den  retunere en Liste af en liste


let findFirstWord (my : hand) (d : dict) ((dx,dy) : coord) (startOfWord : List<coord * (uint32 * (char * int))>) : List<coord * (uint32 * (char * int))> = 
    let rec aux (currentHand : hand) (currentDict : dict) (currentWord : List<coord * (uint32 * (char * int))>) : List<coord * (uint32 * (char * int))>  = 

        //debugPrint("Calling aux\n")
        //debugPrint(sprintf "********currentWord is %A\n" currentWord)
        List.fold(fun (wordSoFar : List<coord * (uint32 * (char * int))>) (currentChar : uint32) -> 
            match Dictionary.step (uintToChar currentChar) currentDict with 
                | None -> 
                    //debugPrint(sprintf "None \n")
                    //debugPrint(sprintf "------------- wordSoFar is = %A \n" wordSoFar)
                    wordSoFar //wordSoFar
                | Some (b, d') ->
                    //debugPrint(sprintf "currentChar is %A\n" (uintToChar currentChar))
                    let newCoord = 
                        if(currentWord.IsEmpty) 
                        then 
                        //Maybe change
                            (0,0)
                        else 
                            ((fst (fst (List.head currentWord))) + dx), ((snd (fst (List.head currentWord))) + dy)

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
                                                  
        ) [] (MultiSet.toList currentHand) 
    aux my d startOfWord

let continueWord (my : hand) (d : dict) (startPositions :  List<(coord*char)*coord>) : List<coord * (uint32 * (char * int))> =
    //debugPrint("calling continueWord aux\n")
    let startPos =(List.rev startPositions)
    debugPrint(sprintf "*****************************")
    debugPrint(sprintf "\nOur hand is %A\n" my)
    debugPrint(sprintf "*****************************")
    //debugPrint(sprintf "startPos are %A\n" startPos)
    if (startPositions.IsEmpty)
    then 
        findFirstWord my d (1,0) [] 
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
                    let result = findFirstWord currentHand d' dir startLetter
                    debugPrint(sprintf "\n\nresult is %A\n\n" result) 
                    //debugPrint(sprintf "result is %A\n" result)
                    let word = result.[0..result.Length-2]
                    debugPrint(sprintf "\n\nword is %A\n\n" word)
                    if (result.Length > word.Length)
                    then
                        debugPrint("inside first if\n") 
                        word
                    elif (result.IsEmpty) then
                        debugPrint("inside second if\n") 
                        debugPrint(sprintf "wordSoFar is %A\n" wordSoFar) 
                        wordSoFar
                    else
                        debugPrint("inside third second if\n") 
                        word
                    
            ) [] startPos //stepper på startPositions
        aux my d

    //Lav  først en ny liste
    //det vi får i findFirstWord giver vi vores accumulator
    //fold over startPositions
    //for hvert af dem skal vi steppe på bogstavet
    //der skal vi bruge  det dictionary vi får til findfirstWord

    //Den list vi får fra findFirstWordSkal vi gemme et nyt sted
    
let placeOnBoard (word : List<coord * (uint32 * (char * int))>) : List<(coord) * (uint32 * (char * int))> =
    //debugPrint("call placeOnBoard \n")
    word
    // let rec aux (word' : List<coord * (uint32 * (char * int))>) acc =
    //     match word' with
    //     | [] -> 
    //         debugPrint("return acc\n")
    //         acc
    //     | _ ->
    //         debugPrint("call placeOnBoard aux againg\n")
    //         //let firstLetter = w[0]
    //         let (coord,(id, (char,point))) = List.head (List.rev word') 
    //         //let pointOfFirstLetter = charToPoint (charToInteger char)
    //         aux word' ((coord,(id, (char, point)))::acc)
    //         //aux word' (x+dx, y+dy) (((x,y), (charToInteger char, (firstLetter, pointOfFirstLetter)))::acc)
    // aux word []

// let placeOnBoard (word : List<coord * (uint32 * (char * int))>) (coordinate: coord) ((dx,dy) : coord) : List<(coord) * (uint32 * (char * int))> =
//     let rec aux (word' : List<coord * (uint32 * (char * int))>) ((x,y): coord) acc =
//         match word' with
//         | [] -> acc
//         | w ->
//             //let firstLetter = w[0]
//             let (_,(_, (char,_))) = List.head (List.rev word') 
//             let pointOfFirstLetter = charToPoint (charToInteger char)
//             aux word' (x+dx, y+dy) (((x,y), (charToInteger char, (char, pointOfFirstLetter)))::acc)
//             //aux word' (x+dx, y+dy) (((x,y), (charToInteger char, (firstLetter, pointOfFirstLetter)))::acc)
//     aux word coordinate []