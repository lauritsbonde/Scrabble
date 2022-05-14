module public Dictionary

    type Dict = N of Map<char, Dict> * bool

    let empty () = N (Map.empty, false)

    let rec insert s (N (map, b)) = 
        match s with
        | "" -> 
            N (map, true)
        | _ ->
            let firstChar = s.Chars 0
            let rest = s.Remove (0,1)
            match Map.tryFind firstChar map with
            | None -> 
                let newMap = Map.add firstChar (insert rest (empty ())) map
                N (newMap, b)
            | Some dict ->
                let newDict = insert rest dict
                N (map.Add (firstChar, newDict), b)

    let rec lookup s (N (map, b)) =
        match s with
        | "" -> 
            b
        | _ ->
            let firstChar = s.Chars 0
            let rest = s.Remove (0,1)
            match Map.tryFind firstChar map with
            | None -> 
                false
            | Some dict ->
                lookup rest dict

