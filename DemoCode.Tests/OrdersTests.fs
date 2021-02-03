namespace Tests

open Xunit
open Swensen.Unquote
open DemoCode.Orders

module ``Adding an item to an order`` =

    let originalItem = { ProductId = 1; Quantity = 1 }

    [<Fact>]
    let ``with same id increases the quantity`` () =
        let order = { Id = 1; Items = [ originalItem ] }
        let newItem = { ProductId = 1; Quantity = 2 }

        test <@ addItem newItem order = { Id = 1; Items = [ { ProductId = 1; Quantity = 3 } ] } @>

    [<Fact>]
    let ``with different id adds the new item and leaves the existing item unchanged`` () =
        let order = { Id = 1; Items = [ originalItem ] }
        let newItem = { ProductId = 2; Quantity = 2 }

        test <@ addItem newItem order = { Id = 1; Items = [ newItem; originalItem ] } @>

module ``Removing an item from an order`` =

    let originalItem = { ProductId = 1; Quantity = 3 }

    [<Fact>]
    let ``leaves the order untouched when the id is not found`` () =
        let order = { Id = 1; Items = [ originalItem ] }
        test <@ removeItem 3 order = order @>

    [<Fact>]
    let ``removes the item entirely`` () =
        let order = { Id = 1; Items = [ originalItem ] }
        test <@ removeItem 1 order = { Id = 1; Items = [] } @>

