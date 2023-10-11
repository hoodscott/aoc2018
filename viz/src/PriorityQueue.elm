module PriorityQueue exposing (insert, peek, pop, test)

{- highest priority goes to front of list -}


type alias PQElement a number =
    { element : a, priority : number }


type alias PriorityQueue a number =
    List (PQElement a number)


insert : number -> a -> PriorityQueue a number -> PriorityQueue a number
insert priority element pq =
    let
        newPQEl =
            PQElement element priority
    in
    case pq of
        head :: [] ->
            if head.priority > priority then
                [ head, newPQEl ]

            else
                [ newPQEl, head ]

        head :: tail ->
            if head.priority > priority then
                head :: insert priority element tail

            else
                [ newPQEl, head ] ++ tail

        [] ->
            [ newPQEl ]


peek : PriorityQueue a number -> Maybe a
peek pq =
    case pq of
        head :: _ ->
            Just head.element

        _ ->
            Nothing


pop : PriorityQueue a number -> ( Maybe a, PriorityQueue a number )
pop pq =
    case pq of
        head :: tail ->
            ( Just head.element, tail )

        _ ->
            ( Nothing, [] )


popuntildone : PriorityQueue a number -> List a
popuntildone pq =
    let
        ( popped, left ) =
            pop pq
    in
    case popped of
        Just element ->
            element :: popuntildone left

        Nothing ->
            []


test : List String
test =
    let
        pq =
            insert 1 "a" []
                |> insert 5 "b"
                |> insert 2 "c"
    in
    popuntildone pq
