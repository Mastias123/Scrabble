// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

 type MultiSet<'a when 'a : comparison> = M of Map<'a, uint32> //M definere constuctoren som man skal bruge som er et Map
    
    let empty = M Map.empty<'a,uint32>

    let isEmpty (M s) = if Map.isEmpty(s) then true else false //vi siger at M har et map s som type

    let size (M s) = Map.fold (fun acc key valu -> acc + valu) 0u s //iterarere henover s og starter på 0. "u" er for at caste
    //vi vil gerne have fat i valuen, derfor plusser vi den på vores accumilator

    let contains a (M s) = 
        match Map.tryFind a s with // none og some fordi det er hvad tryFind returnere  
        | None -> false
        | _ -> true

    let numItems a (M s) =
     match Map.tryFind a s with // none og some fordi det er hvad tryFind returnere  
        | None -> 0u
        | x -> Option.get x //for at fjerne "some" så bruger man Option.get. tryFind kigger igennem mapped og returnere valuen
        //af keyen. Den returnere some hvis der er en value ellers none hvis der ikke er 

    let add a n (M s as ms) = // ms er en forkotelse for multiset
        match numItems a ms with //bruger numItems fra før fordi den returnere hvor mange der er i mapped
        | 0u -> M (Map.add a n s)//første case hvor mapped er tomt derved returnere den 0 fra numItems
        | x -> M (Map.add a (x + n) s) //husk M foran for at den returnere et multisæt istedet for et map. tilføjer 
        //en key med et tal der plusses på min value

    let addSingle a (M s as ms) = 
        match numItems a ms with 
        | 0u -> M (Map.add a 1u s)
        | x -> M (Map.add a (1u + x) s) 

     let remove a n (M s as ms) = 
        match numItems a ms with
        | x when x <= n -> Map.remove a s |> M
        | _ -> add a (0u - n) ms
    // let remove a n (M s as ms) =
    //     match numItems a ms with
    //     | 0u -> ms
    //     | x when n < x -> M (Map.remove a s) //hvis det antal elementer n jeg gerne vil fjerne er større en det antal elementer
    //     //der allerede er x så fjerner jeg keyen og returnere mapped. f.eks. hvis mappet n = 4 > x(2,3) 
    //     | x when n > x -> M (Map.add a (x - n) s) //tilføjer mapped når det antal elementer jeg vil fjerne er mindre end x
    //     // n = 1 < x(2,3) ville blive lig med = 2 fordi 3-1 x = 3 og n = 
        
    let removeSingle a (M s as ms) = remove a 1u ms //remove tjekker også hvis a ikke er i s pga. remove bruger numItems, numItems
    //bruger tryFind der tjekker om den eksistere

    let fold f acc (M s as ms) = Map.fold f acc s //folder sig selv hele tiden

    let foldBack f (M s as ms) acc = Map.foldBack f s acc

    let ofList lst = List.fold (fun acc elm -> addSingle elm acc) empty lst
    let toList (M m as ms) = 
        let rec toList2 a n acc=
            match n with
            | 0u -> acc
            | _ -> toList2 a (n-1u) (a::acc)
        foldBack toList2 ms List.Empty 

    

        
     
