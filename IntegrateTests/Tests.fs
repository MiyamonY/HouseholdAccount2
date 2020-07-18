module IntegratTests

open System
open System.IO
open System.Text
open System.Threading.Tasks
open Microsoft.Data.Sqlite
open Microsoft.Extensions.Primitives
open FSharp.Control.Tasks.V2.ContextInsensitive
open Xunit
open FsUnit
open NSubstitute
open Microsoft.AspNetCore.Http
open Microsoft.EntityFrameworkCore
open Giraffe
open Giraffe.Serialization
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Microsoft.Extensions.Logging

let connectionString = "Data Source=:memory:;"

let next : HttpFunc = Some >> Task.FromResult

let debug = false

let memoryStreamFromString (str : string) =
    new MemoryStream(Encoding.UTF8.GetBytes(str))

let serializeToJson obj =
    let jsonSetting =
        let contractResolver = DefaultContractResolver(NamingStrategy=CamelCaseNamingStrategy())
        JsonSerializerSettings(ContractResolver=contractResolver)
    JsonConvert.SerializeObject(obj, jsonSetting)

let getBody (context : HttpContext) =
    context.Response.Body.Position <- 0L
    use reader = new StreamReader(context.Response.Body, Encoding.UTF8)
    reader.ReadToEnd()

let getStatusCode (context : HttpContext) =
    context.Response.StatusCode

let createAndInitalizeDb (initializeDb : Models.AccountContext -> unit) =
    let keepAliveConnection = new SqliteConnection("DataSource=:memory:")
    keepAliveConnection.Open()

    let factory = LoggerFactory.Create(fun builder -> if debug then builder.AddConsole() |> ignore)
    let options =
        DbContextOptionsBuilder<Models.AccountContext>().UseSqlite(keepAliveConnection)
                                                        .UseLoggerFactory(factory)
                                                        .EnableSensitiveDataLogging(true)
                                                        .Options

    let db = new Models.AccountContext(options)
    db.Database.EnsureCreated() |> ignore
    initializeDb db

    db

let addServices (context : HttpContext) (db : DbContext) =
    context.RequestServices
           .GetService(typeof<Models.AccountContext>)
           .Returns(db) |> ignore
    context.RequestServices
           .GetService(typeof<IJsonSerializer>)
           .Returns(NewtonsoftJsonSerializer(NewtonsoftJsonSerializer.DefaultSettings)) |> ignore
    context.RequestServices
           .GetService(typeof<INegotiationConfig>)
           .Returns(DefaultNegotiationConfig() :> INegotiationConfig) |> ignore


[<Fact>]
let ``Get all accounts`` () =
    let ctx = Substitute.For<HttpContext>()
    ctx.Request.Method.ReturnsForAnyArgs("GET") |> ignore
    ctx.Request.Path.ReturnsForAnyArgs(PathString("/api/v1/accounts")) |> ignore
    ctx.Response.Body <- new MemoryStream()

    let today = DateTime.Today
    let account0 = Models.Account.Create "account0" Models.Type.Foods today 100
    let account1 = Models.Account.Create "account1" Models.Type.Foods today 100
    createAndInitalizeDb (fun context ->
                          context.Accounts.Update account0 |> ignore
                          context.Accounts.Update account1 |> ignore
                          context.SaveChanges true |> ignore)
    |> addServices ctx

    task  {
        let! result = Handlers.routes next ctx

        let expected = serializeToJson [account0;account1]
        result.Value |> getStatusCode |> should equal 200
        result.Value |> getBody |> should equal expected
    }

[<Fact>]
let ``Get an account`` () =
    let ctx = Substitute.For<HttpContext>()
    ctx.Request.Method.ReturnsForAnyArgs("GET") |> ignore
    ctx.Request.Path.ReturnsForAnyArgs(PathString("/api/v1/account/1")) |> ignore
    ctx.Response.Body <- new MemoryStream()

    let today = DateTime.Today
    let account = Models.Account.Create "test" Models.Type.Foods today 100
    createAndInitalizeDb (fun context ->
                          context.Accounts.Update account |> ignore
                          context.SaveChanges true |> ignore)
    |> addServices ctx

    task  {
        let! result = Handlers.routes next ctx

        let expected = serializeToJson account
        result.Value |> getStatusCode |> should equal 200
        result.Value |> getBody |> should equal expected
    }

[<Fact>]
let ``Can't get account if not account existed`` () =
    let ctx = Substitute.For<HttpContext>()
    ctx.Request.Method.ReturnsForAnyArgs("GET") |> ignore
    ctx.Request.Path.ReturnsForAnyArgs(PathString("/api/v1/account/1")) |> ignore
    ctx.Response.Body <- new MemoryStream()

    createAndInitalizeDb (fun context -> ())
    |> addServices ctx

    task {
        let! result = Handlers.routes next ctx

        let expected = """{"error":"account(id=1) not found"}"""
        result.Value |> getBody |> should equal expected
    }

[<Fact>]
let ``Create account`` () =
    let ctx = Substitute.For<HttpContext>()

    let account = Models.Account.Create "test" Models.Type.Foods DateTime.Today 100
    let stream = memoryStreamFromString <| serializeToJson account
    ctx.Request.Body <- stream

    let headers = HeaderDictionary()
    headers.Add("Content-Type", StringValues("application/json"))
    headers.Add("Content-Length", StringValues(stream.Length.ToString()))
    ctx.Request.Headers.ReturnsForAnyArgs headers |> ignore
    ctx.Request.ContentType.ReturnsForAnyArgs "application/json" |> ignore
    ctx.Request.Method.ReturnsForAnyArgs("POST") |> ignore
    ctx.Request.Path.ReturnsForAnyArgs(PathString("/api/v1/account")) |> ignore
    ctx.Response.Body <- new MemoryStream()

    let db = createAndInitalizeDb (fun context -> ())
    addServices ctx db

    task {
        let! result = Handlers.routes next ctx

        result.Value |> getStatusCode |> should equal 204
        query {
            for ac in db.Accounts do
                exists (ac.Name = account.Name && ac.Type = account.Type
                         && ac.UsedDate = account.UsedDate && ac.Amount = account.Amount)
        } |> should be True
    }

[<Fact>]
let ``Update account`` () =
    let ctx = Substitute.For<HttpContext>()

    let updateTo = {Handlers.AccountRequest.Name="updated";
                    Handlers.AccountRequest.Type=Models.Type.Etc;
                    Handlers.AccountRequest.UsedDate=DateTime.Today;
                    Handlers.AccountRequest.Amount=1000}
    let stream = memoryStreamFromString <| serializeToJson updateTo
    ctx.Request.Body <- stream

    let headers = HeaderDictionary()
    headers.Add("Content-Type", StringValues("application/json"))
    headers.Add("Content-Length", StringValues(stream.Length.ToString()))
    ctx.Request.Headers.ReturnsForAnyArgs headers |> ignore
    ctx.Request.ContentType.ReturnsForAnyArgs "application/json" |> ignore
    ctx.Request.Method.ReturnsForAnyArgs("PATCH") |> ignore
    ctx.Request.Path.ReturnsForAnyArgs(PathString("/api/v1/account/1")) |> ignore

    ctx.Response.Body <- new MemoryStream()

    let db = createAndInitalizeDb (fun context ->
                          let account = Models.Account.Create "test" Models.Type.Foods DateTime.Today 100
                          context.Accounts.Update account |> ignore
                          context.SaveChanges true |> ignore)
    addServices ctx db

    task {
        let! result = Handlers.routes next ctx
        result.Value |> getStatusCode |> should equal 204

        query {
            for ac in db.Accounts do
            exists (ac.Name = updateTo.Name && ac.Type = updateTo.Type
                    && ac.UsedDate = updateTo.UsedDate && ac.Amount = updateTo.Amount)
        } |> should be True
    }

[<Fact>]
let ``Can't update account if not account exists`` () =
    let ctx = Substitute.For<HttpContext>()

    let account = Models.Account.Create "test" Models.Type.Foods DateTime.Today 100
    let stream = memoryStreamFromString <| serializeToJson account
    ctx.Request.Body <- stream

    let headers = HeaderDictionary()
    headers.Add("Content-Type", StringValues("application/json"))
    headers.Add("Content-Length", StringValues(stream.Length.ToString()))
    ctx.Request.Headers.ReturnsForAnyArgs headers |> ignore
    ctx.Request.ContentType.ReturnsForAnyArgs "application/json" |> ignore
    ctx.Request.Method.ReturnsForAnyArgs("PATCH") |> ignore
    ctx.Request.Path.ReturnsForAnyArgs(PathString("/api/v1/account/1")) |> ignore

    let body = """{"name":"test0"}"""
    ctx.Response.Body <- new MemoryStream()

    let db = createAndInitalizeDb (fun _ -> ())

    addServices ctx db

    task {
        let! result = Handlers.routes next ctx
        result.Value |> getStatusCode |> should equal 500
        result.Value |> getBody |> should equal """{"error":"account(id=1) not found"}"""
    }


[<Fact>]
let ``Delete account`` () =
    let ctx = Substitute.For<HttpContext>()
    ctx.Request.Method.ReturnsForAnyArgs("DELETE") |> ignore
    ctx.Request.Path.ReturnsForAnyArgs(PathString("/api/v1/account/1")) |> ignore
    ctx.Response.Body <- new MemoryStream()

    let today = DateTime.Today
    let account = Models.Account.Create "test" Models.Type.Foods today 100
    createAndInitalizeDb (fun context ->
                          context.Accounts.Update account |> ignore
                          context.SaveChanges true |> ignore)
    |> addServices ctx

    let app = DELETE >=> routef "/api/v1/account/%i" Handlers.deleteAccount

    task {
        let! result = app next ctx
        result.Value |> getStatusCode |> should equal 204
    }
