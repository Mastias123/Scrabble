namespace NoStepFunction
open MultiSet
open Types
open ScrabbleUtil.DebugPrint
open ScrabbleUtil


module internal HelperFunctions =
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

    let checkIfTileIsEmpty (coordinate : coord) (tiles : tiles): bool =
        match Map.tryFind coordinate tiles with 
        | Some (_) -> 
                    //debugPrint(sprintf "tile is noooooooooot empty at coord: %A\n" coordinate) 
                    false
        | None -> 
                    //debugPrint(sprintf "tile is empty at coord: %A\n" coordinate) 
                    true

    let checkIsOccupied (coordinate : coord) (tiles : tiles) : bool =
        match Map.tryFind coordinate tiles with 
        | Some (_) -> true
        | None -> false
    let checkRightDirection ((x,y) : coord) (tiles : tiles) (dx, dy) : bool = 
        let emptyResult = checkIfTileIsEmpty (x+dx, y+dy) tiles
        emptyResult

    let checkDownDirection ((x,y) : coord) (tiles : tiles) (dx, dy) : bool = 
        let emptyResult = checkIfTileIsEmpty (x+dx, y+dy) tiles
        emptyResult
    
    let checkDownLeftDirection ((x,y) : coord) (tiles : tiles) (dx, dy) : bool = 
        let emptyResult = checkIfTileIsEmpty (x+dx, y+dy) tiles
        emptyResult

    let checkUpRightDirection ((x,y) : coord) (tiles : tiles) (dx, dy) : bool = 
        let emptyResult = checkIfTileIsEmpty (x+dx, y+dy) tiles
        emptyResult

    let checkDirection ((x,y) : coord) (tiles : tiles) (dx, dy) : bool =
        //debugPrint(sprintf "checking direction at %A\n" (x,y))
        let emptyResult = checkIfTileIsEmpty (x+dx, y+dy) tiles && checkIfTileIsEmpty (x-dx, y-dy) tiles
        //let notEmptyResult = checkIsOccupied (x+dx, y-dy) tiles
        emptyResult
        //we look to the right and the left
    
    //checkIsOccupied (x+dx, y+dy) tiles && 

    