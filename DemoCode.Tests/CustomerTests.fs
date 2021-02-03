namespace Tests

open Xunit
open Swensen.Unquote
open DemoCode.CustomerTypes
open DemoCode.Customer

module ``Convert customer to eligible`` =

    let sourceCustomer = { CustomerId = "John"; IsRegistered = true; IsEligible = true }

    [<Fact>]
    let ``should succeed if not currently eligible`` () =
        let customer = { sourceCustomer with IsEligible = false }
        let upgraded = convertToEligible customer
        test <@ customer <> upgraded @>

    [<Fact>]
    let ``should return eligible customer unchanged`` () =
        let upgraded = convertToEligible sourceCustomer
        test <@ upgraded = sourceCustomer @>
        
module ``Create customer`` =

    let name = "John"

    [<Fact>]
    let ``should succeed if customer does not exist`` () =
        let existing = None
        let result = (name, existing) ||> tryCreateCustomer 
        test <@ result = Ok({ CustomerId = name; IsRegistered = true; IsEligible = false }) @>

    [<Fact>]
    let ``should fail if customer does exist`` () =
        let existing = Some { CustomerId = name; IsRegistered = true; IsEligible = false }
        let result = (name, existing) ||> tryCreateCustomer 
        test <@ result = Error(CustomerAlreadyExistsException) @>
