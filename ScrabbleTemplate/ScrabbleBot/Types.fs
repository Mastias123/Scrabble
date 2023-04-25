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
