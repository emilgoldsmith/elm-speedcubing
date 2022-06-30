module List.Nonempty.Extra exposing (find, getAt, lift2, lift3, minimum, removeAt)

import List.Extra
import List.Nonempty


lift2 :
    (a -> b -> c)
    -> List.Nonempty.Nonempty a
    -> List.Nonempty.Nonempty b
    -> List.Nonempty.Nonempty c
lift2 f la lb =
    la
        |> List.Nonempty.concatMap
            (\a -> lb |> List.Nonempty.map (\b -> f a b))


lift3 :
    (a -> b -> c -> d)
    -> List.Nonempty.Nonempty a
    -> List.Nonempty.Nonempty b
    -> List.Nonempty.Nonempty c
    -> List.Nonempty.Nonempty d
lift3 f la lb lc =
    la
        |> List.Nonempty.concatMap
            (\a ->
                lb
                    |> List.Nonempty.concatMap
                        (\b ->
                            lc
                                |> List.Nonempty.map (\c -> f a b c)
                        )
            )


minimum : List.Nonempty.Nonempty comparable -> comparable
minimum =
    List.Nonempty.foldl1 min


find : (a -> Bool) -> List.Nonempty.Nonempty a -> Maybe a
find f list =
    list
        |> List.Nonempty.toList
        |> List.Extra.find f


removeAt : Int -> List.Nonempty.Nonempty a -> Maybe (List.Nonempty.Nonempty a)
removeAt index list =
    list
        |> List.Nonempty.toList
        |> List.Extra.removeAt index
        |> List.Nonempty.fromList


getAt : Int -> List.Nonempty.Nonempty a -> Maybe a
getAt index list =
    list
        |> List.Nonempty.toList
        |> List.Extra.getAt index
