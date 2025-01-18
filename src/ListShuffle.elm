module ListShuffle exposing (..)

import List.Extra as List
import Random


shuffleList : Random.Seed -> List a -> ( List a, Random.Seed )
shuffleList seed list =
    shuffleListHelper seed list


shuffleListHelper : Random.Seed -> List a -> ( List a, Random.Seed )
shuffleListHelper seed unshuffled =
    unshuffled
        |> List.foldl
            (\default ( remainingItems, shuffledList, currentSeed ) ->
                case remainingItems of
                    [] ->
                        ( remainingItems, shuffledList, currentSeed )

                    _ ->
                        -- pick a random item from list which will also give a new seed,
                        -- add it to shuffled list,
                        let
                            indexGenerator =
                                Random.int 0 (List.length remainingItems - 1)

                            ( index, newSeed ) =
                                Random.step indexGenerator seed

                            newItem =
                                List.getAt index remainingItems
                                    |> Maybe.withDefault default

                            newRemainingItems =
                                List.removeAt index remainingItems

                            --
                        in
                        ( newRemainingItems, newItem :: shuffledList, newSeed )
            )
            ( unshuffled, [], seed )
        |> (\( _, a, newSeed ) -> ( a, newSeed ))
