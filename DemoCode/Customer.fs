namespace DemoCode

[<AutoOpen>]
module CustomerTypes =
    type Customer = { 
        CustomerId : string
        IsRegistered : bool
        IsEligible : bool 
    }

    type CreateCustomerException =
        | RemoteServerException of exn
        | CustomerAlreadyExistsException

module Db =
    let tryGetCustomer customerId = // String -> Result<Customer option,CreateCustomerException>
        try
            [
                { CustomerId = "John"; IsRegistered = true; IsEligible = true }
                { CustomerId = "Mary"; IsRegistered = true; IsEligible = true }
                { CustomerId = "Richard"; IsRegistered = true; IsEligible = false }
                { CustomerId = "Sarah"; IsRegistered = false; IsEligible = false }
            ]
            |> List.tryFind (fun c -> c.CustomerId = customerId)
            |> Ok
        with
        | ex -> Error (RemoteServerException ex)

    let saveCustomer (customer:Customer) = // Customer -> unit
        try
            //Save customer
            Ok ()
        with
        | ex -> Error (RemoteServerException ex)

module Customer =
    let convertToEligible customer = // Customer -> Customer
        if not customer.IsEligible then { customer with IsEligible = true }
        else customer

    let createCustomer customerId = { CustomerId = customerId; IsRegistered = true; IsEligible = false }

    let tryConvertToEligible convertToEligible customer =
        match customer with
        | Some c -> Some (convertToEligible c)
        | None -> None

    let trySaveCustomer saveCustomer customer = // (Customer -> Result<unit,CreateCustomerException>) -> Customer option -> Result<unit,CreateCustomerException>
        match customer with
        | Some c -> c |> saveCustomer
        | None -> Ok ()

    let tryCreateCustomer customerId (customer:Customer option) =
        match customer with
        | Some _ -> Error CustomerAlreadyExistsException
        | None -> Ok (createCustomer customerId)

    let upgradeCustomer customerId =
            customerId
            |> Db.tryGetCustomer 
            |> Result.map (Option.map convertToEligible)
            |> Result.bind (trySaveCustomer Db.saveCustomer)

    let registerCustomer customerId =
        customerId
        |> Db.tryGetCustomer 
        |> Result.bind (tryCreateCustomer customerId)
        |> Result.bind Db.saveCustomer

