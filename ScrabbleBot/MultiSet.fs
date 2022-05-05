module internal MultiSet

    type MultiSet<'a> when 'a : comparison = Ms of Map<'a, uint32>

    let empty = Ms (Map.empty<'a, uint32>)

    let isEmpty (Ms map) = Map.isEmpty map

    let size (Ms map) = Map.fold (fun acc key value -> acc + value ) 0u map 

    let contains x (Ms map) = Map.containsKey x map

    let numItems x (Ms map) = Map.tryFind x map |> Option.defaultValue 0u

    let add a n (Ms map) = 
        let old = Map.tryFind a map |> Option.defaultValue 0u
        Ms (map.Add (a, (old + n)))

    let addSingle a MS = add a 1u MS

    let remove a n (Ms map) =
        let old = Map.tryFind a map |> Option.defaultValue 0u
        if((int old) - (int n) < 0) then
            Ms (map.Remove a)
        else
            Ms (map.Add (a, (old - n)))
    
    let removeSingle a MS = remove a 1u MS

    let fold f acc (Ms map) = Map.fold f acc map;

    let foldBack f (Ms map) acc = Map.foldBack f map acc;

    let ofList lst = List.fold (fun acc string -> addSingle string acc) empty lst

    let toList ms = fold (fun acc key value -> acc @ List.replicate (int value) key) List.empty ms
    
    let map f ms = fold (fun acc key value -> add (f key) value acc) empty ms

    let union ms1 (Ms ms2) =
        fold (
            fun acc key value -> 
            let max2 = Map.tryFind key ms2 |> Option.defaultValue 0u
            if (max2 > value) then
                add key max2 acc
            else 
                add key value acc
        ) empty ms1

    let sum ms1 (Ms ms2) =
        fold (
            fun acc key value -> 
            let max2 = Map.tryFind key ms2 |> Option.defaultValue 0u
            add key (value+max2) acc
        ) empty ms1

    let subtract ms1 (Ms ms2) =
        fold (
            fun acc key value -> 
            let max2 = Map.tryFind key ms2 |> Option.defaultValue 0u
            remove key max2 acc
        ) empty ms1

    let intersection ms1 (Ms ms2) = 
        fold (
            fun acc key value ->
            if Map.containsKey key ms2 then
                add key value acc
            else empty
        ) empty ms1