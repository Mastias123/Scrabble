// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad
    open ScrabbleUtil


    (* Code for testing *)
    let hello = [('H', 4);('E', 1);('L', 1);('L', 1);('O', 1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
      
    let add a b = a >>= (fun x -> b >>= (fun y -> ret (x + y)))
    let sub a b = a >>= (fun x -> b >>= (fun y -> ret (x - y)))
    let mul a b = a >>= (fun x -> b >>= (fun y -> ret (x * y)))
    let div a b = a >>= (fun x -> b >>= (fun y -> if(y = 0) then fail DivisionByZero else ret (x / y)))   
    let modu a b = a >>= (fun x -> b >>= (fun y -> if(y = 0) then fail DivisionByZero else ret (x % y)))
    let (|=|) a b = a >>= (fun x -> b >>= (fun y -> ret (x = y)))
    let (|<|) a b = a >>= (fun x -> b >>= (fun y -> ret (x < y)))
    let conj a b = a >>= (fun x -> b >>= (fun y ->  ret(x && y)))
    // let conj a b = a >>=(fun x -> b >>= (fun y -> if(x=true && y=true) then ret(true) else ret(false)))
    let isVowel = 
        function
        |'a'|'e'|'i'|'o'|'u'|'A'|'E'|'I'|'O'|'U' -> ret(true)
        | _ -> ret(false)
    
    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> = 
        match a with
        | N n -> ret (n)
        | V v -> lookup v
        | WL -> wordLength
        | PV pv -> arithEval pv >>= pointValue 
        | Add (x, y) -> add (arithEval x) (arithEval y)
        | Sub (x, y) -> sub (arithEval x) (arithEval y)
        | Mul (x, y) -> mul (arithEval x) (arithEval y)
        | Div (x, y) -> div (arithEval x) (arithEval y)
        | Mod (x, y) -> modu (arithEval x) (arithEval y)
        | CharToInt c -> charEval c >>= (fun r -> ret (int r))

    and charEval c : SM<char> =  
        match c with
        | C c -> ret(c)  
        | CV cv -> arithEval cv >>= characterValue  
        | ToUpper c -> charEval c >>= (fun r -> ret (System.Char.ToUpper r))
        | ToLower c -> charEval c >>= (fun r -> ret (System.Char.ToLower r))
        | IntToChar a -> arithEval a >>= (fun r -> ret (char r))

    and boolEval b : SM<bool> = 
        match b with
        | TT -> ret (true)
        | FF -> ret (false)
        | AEq (a, b) -> arithEval a |=| arithEval b
        | ALt (a, b) -> arithEval a |<| arithEval b
        | Not b -> boolEval b >>= (fun r -> ret (not r))
        | Conj (a, b) -> conj (boolEval a) (boolEval b)
        | IsVowel c -> charEval c >>= (fun r -> isVowel r)
        | IsLetter c -> charEval c >>= (fun r -> ret (System.Char.IsLetter r))
        | IsDigit c -> charEval c >>= (fun r -> ret (System.Char.IsDigit r))



    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }


    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    
    
