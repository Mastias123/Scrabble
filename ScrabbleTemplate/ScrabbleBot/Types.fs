namespace NoStepFunction
open MultiSet
open ScrabbleUtil

module internal Types =
    //board
    type tiles= Map<int*int, char>

    //hand
    type hand = MultiSet<char>

    type myTurn = bool
    type dict = Dictionary.Dict