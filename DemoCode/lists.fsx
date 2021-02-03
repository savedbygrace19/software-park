let items = [ for x in 1..5 do yield x ]

let items' = 6 :: items

let readList items =
    match items with
    | [] -> sprintf "Empty list"
    | head :: tail -> sprintf "Head: %A and Tail: %A" head tail

let emptyList = readList [] // "Empty list"
let multipleList = readList [1;2;3;4;5] // "Head: 1 and Tail: [2;3;4;5]"
let singleItemList = readList [1] // "Head: 1 and Tail: []"

let list1 = [1..5]
let list2 = [3..7]
let emptyList' = []

let joinedEmpty = list1 @ emptyList' // [1;2;3;4;5]
let emptyJoined = emptyList' @ list1 // [1;2;3;4;5]

let joined = list1 @ list2 // [1;2;3;4;5;3;4;5;6;7]
let joined' = List.concat [list1;list2]

let myList = [1..9]

let getEvens items =
    items
    |> List.filter (fun x -> x % 2 = 0)

let evens = getEvens myList // [2;4;6;8]

let sum items =
    items |> List.sum

let mySum = sum myList // 45

let triple items =
    items
    |> List.map (fun x -> x * 3)

let myTriples = triple myList

let print items =
    items
    |> List.iter (fun x -> (printfn "My value is %i" x))

print myList |> ignore

let itemsAgain = [(1,0.25M);(5,0.25M);(1,2.25M);(1,125M);(7,10.9M)]

let sumCombos items =
    items
    |> List.sumBy (fun (x,y) -> decimal x * y)

let sumResult = sumCombos itemsAgain

let getTotal items =
    items
    |> List.fold (fun acc (q, p) -> acc + decimal q * p) 0M

let getTotalPutDifferently items =
    (0M, items) ||> List.fold (fun acc (q, p) -> acc + decimal q * p)

let total = getTotal itemsAgain
