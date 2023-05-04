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



//my is the hand we have with the tiles,  d is the trie, coord is the coordinates on the board, dir is the upcoming directions on the board
let findFirstWord (my : hand) (d : dict) (coord : coord) (dir : coord) : List<coord * (uint32 * (char * int))> =
    let rec aux (currentHand : hand) (currentDict : dict) (currentWord : List<coord * (uint32 * (char * int))>) = 
        
        List.fold (fun (wordSoFar : List<coord * (uint32 * (char * int))>) (c : uint32) -> 
            match Dictionary.step (uintToChar c) currentDict with 
                | Some (b, d') -> //when we get a char  
                    //debugPrint(sprintf "******Looking at  letter %A\n" c)
                    
                    //debugPrint(sprintf "Trying to remove new hand: %A\n" wordSoFar)
                    let newhand = MultiSet.removeSingle c currentHand //removes the char from hand so we can look at the next
                    //Use tile so when we call aux then give the next coordinat with an direction
                    
                    //Get old coordinate and direction
                    //
                    let (oldCoord, _) = List.head (List.rev currentWord)
                    let (_,(oldId, (oldChar,oldPoint))) = List.head (List.rev currentWord)
                    //Build list with coordinate id char and points
                    let currentString = (oldCoord, (oldId,(oldChar,oldPoint)))::currentWord //current string is the string build so far from the acc 
                    //debugPrint(sprintf "-----------------CurrentString is %A --------- CurrentWord is %A\n" currentString currentWord)
                    //let currentWordString = List.fold (fun acc (_,(_,(ch,_)) ) -> ch::acc) [] currentString |> List.rev |> List.toArray |> System.String.Concat
                    let LongestWordInBranch = aux (newhand) d' currentString //gives the new hand with the string
                    //debugPrint(sprintf "Succesfull remove new hand: %A\n" wordSoFar)
                    if(b && currentString.Length > LongestWordInBranch.Length && currentString.Length > wordSoFar.Length) //
                    then
                        //Then return our string
                        //debugPrint( sprintf "The boolean is true, current word is %A\n" (currentString))
                        currentString 
                        //currentWordString
                    elif (LongestWordInBranch.Length > currentWord.Length) then
                        //debugPrint( sprintf "The boolean is true when word > currentword, current word is %A\n" (currentString))
                        LongestWordInBranch
                        //currentString
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
        
        ) [] (MultiSet.toList currentHand)      
    aux my d []

let continueNextWord (hand: hand) (dic: dict) (startpositions : List<(coord*char)*(coord)>) = 

    List.fold (fun acc ((coord, _), dir) -> 
        let (x,y) = coord
        let (dx,dy) = dir
        let newCoord = (x+dx, y+dy)
        let newDir = dir
        let newWord = findFirstWord hand dic newCoord newDir
        newWord::acc
    ) [] startpositions
    //Call fold over startPositions
    //Call step function on each startposition
    //Find first word on each startPosition
    //Find all possible words on each startPosition
    //

let placeOnBoard (word : List<coord * (uint32 * (char * int))>) (coordinate: coord) ((dx,dy) : coord) : List<(coord) * (uint32 * (char * int))> =
    let rec aux (word' : List<coord * (uint32 * (char * int))>) ((x,y): coord) acc =
        match word' with
        | [] -> acc
        | w ->
            //let firstLetter = w[0]
            let (_,(_, (char,_))) = List.head (List.rev word') 
            let pointOfFirstLetter = charToPoint (charToInteger char)
            aux word' (x+dx, y+dy) (((x,y), (charToInteger char, (char, pointOfFirstLetter)))::acc)
            //aux word' (x+dx, y+dy) (((x,y), (charToInteger char, (firstLetter, pointOfFirstLetter)))::acc)
    aux word coordinate []

    //Then try to create a word out of the first word
    //let wordsToPlay = Dictionary.step firstTile d
    
        
