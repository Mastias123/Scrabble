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
        Map.fold(fun acc coord c -> 
            //debugPrint(sprintf "Looking at cord %A and char %A\n" coord c)
            match checkDirection coord tiles right with
            | true -> 
                //debugPrint("True\n")
                ((coord, c), right)::acc
            | false -> 
                //debugPrint("True\n")
                acc
        ) [] tiles
    
    let vetical =
        Map.fold(fun acc1 coord1 char1 -> 
            match checkDirection coord1 tiles down with 
            | true -> ((coord1, char1), down)::acc1
            | false -> acc1
        ) [] tiles
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
        debugPrint(sprintf"newStartPos = %A\n" newStartPosAndDir)
        newStartPosAndDir
let getRandomStartLetter list =
        let rnd = System.Random()
        let index = rnd.Next(0, List.length list)
        List.item index list

let findFirstWord (my : hand) (d : dict) ((x,y) : coord) ((dx,dy) : coord) (startPositions :List<(coord*char)*(coord)>) : List<coord * (uint32 * (char * int))> =   
    debugPrint(sprintf "startPositions are %A\n\n" startPositions)
    let startLetter  =
        List.fold (fun acc ((tempCoord, tempChar), _) -> 
            match Dictionary.step tempChar d with 
                | None -> acc
                | Some (b, _) -> 
                    if(b)
                    then
                        acc
                    else 
                        let pointOfFirstLetter = charToPoint (charToInteger tempChar)
                        let newStartLetter = (tempCoord,((charToInteger tempChar), (tempChar, pointOfFirstLetter)))::acc
                        debugPrint(sprintf "newStartLetter is %A \n" newStartLetter)
                        newStartLetter                       
        ) [] startPositions
        
    //debugPrint(sprintf "**** startLetter is = %A\n" startLetter)
    debugPrint(sprintf "coordinate is = %A\n" (x,y))
    debugPrint(sprintf "direction is = %A\n" (dx,dy))
    

    let rec aux (currentHand : hand) (currentDict : dict) (currentWord : List<coord * (uint32 * (char * int))>) : List<coord * (uint32 * (char * int))>  = //currentWord er startListen
        //debugPrint("calling aux\n")
            // | H | E | J |
            // |   |   | E |
            // |   |   | G |
            // CurrentWord = [J,..]
        let newCoord = 
            if(currentWord.IsEmpty) 
            then 
            //Maybe change
                (0,0)
            else 
                ((fst (fst (List.head currentWord))) + dx), ((snd (fst (List.head currentWord))) + dy)
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
                                                  
        ) startLetter (MultiSet.toList currentHand) 
    aux my d startLetter
    
let placeOnBoard (word : List<coord * (uint32 * (char * int))>) : List<(coord) * (uint32 * (char * int))> =
    debugPrint("call placeOnBoard \n")
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


    
        
