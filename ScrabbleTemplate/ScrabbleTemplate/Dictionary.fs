// Trie implementation
module internal Dictionary
    open System.Collections.Generic
    type Dict =
        | Leaf of bool
        | Node of bool * Dictionary<char, Dict>
    let empty () = Leaf false //returns an empty dictionary

    let rec insert (s:string)  = 
        function
        | Leaf _ when s.Length = 0 -> Leaf true // Base case when string length is equal to 0 
        | Node (_, dict) when s.Length = 0 -> Node(true, dict)

        | Leaf (b:bool) -> // Turns leaf into node with first char in the substring.
            let fst = s.[0]
            let rest = s.[1..]
            let dict = Dictionary ()
            dict.[fst] <- insert rest (empty ())
            Node (b, dict)

        | Node (b, dict) -> 
            let fst = s.[0]
            let rest = s.[1..]

            if dict.ContainsKey fst then // if there are elements in the dictionary, if the char is already in the node then we get the value
                let dic = dict.GetValueOrDefault fst
                dict.[fst] <- insert rest (dic)
                Node (b, dict)
            else // If there are no other elements in the dictionary
                dict.[fst] <- insert rest (empty())
                Node (b, dict)    
    
    let rec lookup (s: string) = 
        function 
        | Leaf b when s.Length = 0 -> b
        | Leaf _ -> false
        | Node (b,_) when s.Length = 0 -> b
        | Node (_,dict) ->
            let fst = s.[0]
            let rest = s.[1..]
            
            if dict.ContainsKey fst then //checks if the dictionary contains the first char, if true then returns the dictionary with the char
                let dic = dict.GetValueOrDefault fst
                lookup rest dic
            else
                false

    let step (c: char) (dict: Dict) = 
        match dict with
        | Leaf _ -> None //cannot come further down than a leaf
        | Node (_, map) -> 
            match map.TryGetValue c with //match if we have the char value
            | (false, _) -> None
            | (true, value) -> //check if there is another layer in the trie
                match value with  
                | Leaf b -> Some (b, value)
                | Node (b, _) -> Some (b, value)
                //if we return some then we can build the word given the char else if none is returned then go to next letter

            