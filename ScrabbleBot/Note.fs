
    // let firstMove (st: State.state) (pieces : Map<uint32, tile>) =
    //     let rec aux curHand curDict (currentWord: string) (bestword: string) = 
    //         debugPrint (sprintf "Current Word: %A - Best Word %A\n" currentWord bestword)
    //         MultiSet.fold (fun _ x _ ->
    //             (Set.fold (fun _ (c, _) -> 
    //                 match Dictionary.step c curDict with
    //                 | None -> 
    //                     debugPrint (sprintf "No word found for %A - bestword %A\n" c bestword)
    //                     bestword
    //                 | Some (isWord, newDict) -> 
    //                     let newHand = MultiSet.removeSingle x curHand
    //                     let newWord = currentWord + (string c)
    //                     if isWord && newWord.Length > bestword.Length then
    //                         debugPrint (sprintf "New Best Word: %A\n" newWord)
    //                         aux newHand newDict newWord newWord
    //                     else
    //                         debugPrint (sprintf "bestword in else %A\n" bestword)
    //                         aux newHand newDict newWord bestword
    //                 ) "" (Map.find x pieces)
    //             )
    //             debugPrint (sprintf "bestword in fold %A\n" bestword)
    //         ) () curHand
    //         bestword
    //     aux st.hand st.dict "" ""