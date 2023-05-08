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

    let right = (1,0) : coord
    let down = (0,1) : coord

    