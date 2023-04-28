namespace NoStepFunction
open MultiSet
open ScrabbleUtil

module internal Types =
    //board
    type tiles= Map<int*int, char>

    //hand
    type hand = MultiSet.MultiSet<uint32>

    type myTurn = bool
    type dict = Dictionary.Dict

    let uintToChar uint = char(uint + 64u)

    let charToInteger char = 
        if (char = '?') then 0u
        else uint32(System.Char.ToUpper(char)) - 64u

    
    let charToPoint (id: uint32) : int =
        match id with
        | 0u                                             -> 0
        | 1u | 5u | 9u | 12u | 14u | 15u | 18u | 19u | 20u | 21u  -> 1
        | 4u | 7u                                         -> 2
        | 2u | 3u | 13u | 16u                               -> 3
        | 6u | 8u | 22u | 23u | 25u                          -> 4
        | 11u                                            -> 5
        | 10u | 24u                                      -> 8
        | 17u | 26u                                       -> 10
        | _                                             -> failwith "Not valid character index"

