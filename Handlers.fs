module Handlers

open System
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open Newtonsoft.Json

type Response =
    {
        Error: string
    }

let getDBContext (context : HttpContext) =
    context.GetService<Models.AccountContext>()

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occured")
    match ex with
    | :? JsonReaderException as ex -> clearResponse >=> RequestErrors.BAD_REQUEST { Error="invalid request"}
    | _ -> clearResponse >=> ServerErrors.INTERNAL_ERROR ex.Message


let getAccount (id : int) : HttpHandler =
    fun (next : HttpFunc) (context : HttpContext) ->
        let dbContext = getDBContext context
        task {
            let! result = Models.getAccount dbContext id
            match result with
            | Ok account -> return! account |> context.WriteJsonAsync
            | Error msg -> return! ServerErrors.INTERNAL_ERROR { Error = msg } next context
        }

[<CLIMutable>]
type IntervalRequest =
    {
        From: DateTime
        To: DateTime
    }

let getAccounts : HttpHandler =
    fun (next:HttpFunc) (context:HttpContext) ->
        let dbContext = getDBContext context
        task {
            match context.TryGetQueryStringValue "from", context.TryGetQueryStringValue "to" with
            | None, None ->
                let accounts = Models.getAccounts dbContext
                return! context.WriteJsonAsync accounts
            | Some _, Some _ ->
                let interval = context.BindQueryString<IntervalRequest>()
                let accounts = Models.getAccountsInterval dbContext interval.From interval.To
                return! context.WriteJsonAsync accounts
            | _, _ ->  return! RequestErrors.BAD_REQUEST { Error = "invalid query paramters" } next context
        }

[<CLIMutable>]
type AccountRequest =
    {
        Name: String
        Type: Models.Type
        UsedDate: DateTime
        Amount: int
    }

    member this.BindModelAccount () =
        Models.Account.Create this.Name this.Type this.UsedDate this.Amount


let addAccount : HttpHandler =
    fun (next:HttpFunc) (context:HttpContext) ->
        let dbContext = getDBContext context
        task {
            let! req = context.BindModelAsync<AccountRequest>()

            let! result = Models.addAccount dbContext <| req.BindModelAccount ()
            return! (match result with
                     | Ok _ -> Successful.NO_CONTENT
                     | Error msg -> ServerErrors.INTERNAL_ERROR { Error=msg }) next context
        }

let updateAccount (id:int) : HttpHandler =
    fun (next:HttpFunc) (context:HttpContext) ->
        let dbContext = getDBContext context
        task {
            let! req = context.BindModelAsync<AccountRequest>()

            let! result = Models.updateAccount dbContext id <| req.BindModelAccount ()
            return! (match result with
                     | Ok _ -> Successful.NO_CONTENT
                     | Error msg -> ServerErrors.INTERNAL_ERROR {Error=msg}) next context
        }

let deleteAccount (id:int) : HttpHandler =
    fun (next : HttpFunc) (context : HttpContext) ->
        let dbContext = getDBContext context
        task {
            let! result = Models.deleteAccount dbContext id
            return! (match result with
                     | Ok _ -> Successful.NO_CONTENT
                     | Error msg -> RequestErrors.BAD_REQUEST {Error = msg}) next context
        }

let handler: HttpHandler =
    choose [GET >=> choose [route "/api/v1/accounts" >=> getAccounts;
                            routef "/api/v1/account/%i" getAccount];
            POST >=> route "/api/v1/account" >=> addAccount;
            PATCH >=> routef "/api/v1/account/%i" updateAccount;
            DELETE >=> routef "/api/v1/account/%i" deleteAccount]
