namespace DemoCode

module Orders =

    type Item = {
        ProductId : int
        Quantity : int
    }

    type Order = {
        Id : int
        Items : Item list
    }

    let recalculateItems items = // Item list -> Item list
        items
        |> List.groupBy (fun i -> i.ProductId)
        |> List.map (fun (id, items) -> { ProductId = id; Quantity = items |> List.sumBy (fun i -> i.Quantity) })

    let addItem item order =
        { order with Items = item :: order.Items |> recalculateItems }

    let addItems items order =
        { order with Items = items @ order.Items |> recalculateItems }

    let removeItem productId order =
        { order with Items = order.Items |> List.filter (fun x -> x.ProductId <> productId) }

    let reduceItem productId quantity order =
        let items = 
            { ProductId = productId; Quantity = -quantity } :: order.Items
            |> recalculateItems
            |> List.filter (fun x -> x.Quantity > 0)
        { order with Items = items }
    
    let clearItems order = 
        { order with Items = [] }