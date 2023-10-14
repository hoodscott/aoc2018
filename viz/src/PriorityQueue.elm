module PriorityQueue exposing (PQElement, PriorityQueue, getElement, getPriority, insert, peek, pop, test)

{- highest priority goes to front of list -}


type alias PQElement a number =
    { element : a, priority : number }


type alias PriorityQueue a number =
    List (PQElement a number)


insert : number -> a -> PriorityQueue a number -> PriorityQueue a number
insert priority element queue =
    let
        newPQEl =
            PQElement element priority
    in
    case queue of
        head :: [] ->
            if priority < head.priority then
                [ head, newPQEl ]

            else
                [ newPQEl, head ]

        head :: tail ->
            if priority < head.priority then
                head :: insert priority element tail

            else
                [ newPQEl, head ] ++ tail

        [] ->
            [ newPQEl ]


peek : PriorityQueue a number -> Maybe a
peek queue =
    case queue of
        head :: _ ->
            Just head.element

        _ ->
            Nothing


pop : PriorityQueue a number -> ( Maybe a, PriorityQueue a number )
pop queue =
    case queue of
        head :: tail ->
            ( Just head.element, tail )

        _ ->
            ( Nothing, [] )


getPriority : PQElement a number -> number
getPriority element =
    .priority element


getElement : PQElement a number -> a
getElement element =
    .element element


popuntildone : PriorityQueue a number -> List a
popuntildone queue =
    let
        ( popped, left ) =
            pop queue
    in
    case popped of
        Just element ->
            element :: popuntildone left

        Nothing ->
            []


test : List String
test =
    let
        queue =
            insert 1 "a" []
                |> insert 5 "b"
                |> insert 2 "c"
    in
    popuntildone queue
