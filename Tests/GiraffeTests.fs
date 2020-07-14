module GiraffeTests

open System
open System.IO
open System.Text
open System.Threading.Tasks
open Xunit
open FsUnit

open Giraffe
open NSubstitute
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open FSharp.Control.Tasks
open Microsoft.Extensions.Primitives
open System.Collections.Specialized

let getBody (ctx : HttpContext) =
    ctx.Response.Body.Position <- 0L
    use reader = new StreamReader(ctx.Response.Body, Encoding.UTF8)
    reader.ReadToEnd()

let getHeader (ctx: HttpContext) key =
    ctx.Response.Headers.[key]

let getStatusCode (ctx : HttpContext) =
    ctx.Response.StatusCode

let next : HttpFunc = Some >> Task.FromResult

let TestCannotExecute () = true |> should be True

let createContext (method_:string) path =
    let ctx = Substitute.For<HttpContext>()
    ctx.Request.PathBase.Returns(PathString("/")) |> ignore
    ctx.Request.Path.Returns(PathString(path)) |> ignore
    let queryStr = "?name=MiyamonY"
    let query = Microsoft.AspNetCore.WebUtilities.QueryHelpers.ParseQuery queryStr
    ctx.Request.Query.Returns(QueryCollection(query) :> IQueryCollection) |> ignore
    ctx.Request.Method.ReturnsForAnyArgs method_ |> ignore
    let headers = HeaderDictionary()
    headers.Add("X-MyOwnHeader", StringValues("abc"))
    ctx.Request.Headers.Returns(headers) |> ignore
    let data = """{"Name": "aaa", "Make":"aaa", "Wheels":3}"""
    ctx.Request.Body.Returns(new MemoryStream(Encoding.ASCII.GetBytes(data))) |> ignore
    ctx.Response.Body <- new MemoryStream()
    ctx

[<Fact>]
let ``Combinator combines HttpHandlers`` () =
    let app = (GET
      >=> route "/"
      >=> setHttpHeader "Content-Type" "text/plain"
      >=> setStatusCode 200
      >=> setBodyFromString "Hello World")

    let ctx = createContext "GET" "/"
    task {
        let! result = app next ctx
        let ctx = result.Value
        ctx.Response.StatusCode |> should equal 200
        ctx.Response.Headers.["Content-Type"] |> should equal "text/plain"
        getBody ctx |> should equal "Hello World" }


[<Fact>]
let ``Choose selects HttpHandlers`` () =
    let app = (GET >=> choose [route "/foo" >=> text "Foo";
                               route "/bar" >=> text "bar"])

    let ctx = createContext "GET" "/bar"
    task {
        let! result = app next ctx
        let ctx = result.Value
        getBody ctx |> should equal "bar"}


[<Fact>]
let ``Warbler evaluates when accessed`` () =
    let time () = DateTimeOffset.Now.ToUnixTimeMilliseconds ()
    let app = (GET >=> choose [route "/warbler" >=> warbler (fun _ ->
                                                             text (time () |> string))])

    let ctxWarbler = createContext "GET" "/warbler"
    let ctxWarbler2 = createContext "GET" "/warbler"
    task {
        let! respWarbler = app next ctxWarbler
        let! _ = Task.Delay(10)
        let! respWarbler2 = app next ctxWarbler2
        getBody respWarbler.Value |> should not' (equal (getBody respWarbler2.Value)) }

[<Fact>]
let ``When creating new task, use task computation expression`` () =
    let personHandler (next: HttpFunc) (ctx:HttpContext) =
        task {
            return! text "Hello World" next ctx }

    let app = (GET >=> route "/" >=> personHandler)

    let ctx = createContext "GET" "/"
    task {
        let! resp = app next ctx
        getBody resp.Value |> should equal "Hello World"
    }

[<Fact>]
let ``Creating new HttpHandler`` () =
    let sayHelloWorld (name :string) : HttpHandler =
        let greeting = sprintf "Hello World, from %s" name
        text greeting

    let app = (GET >=> route "/" >=> sayHelloWorld "MiyamonY")

    let ctx = createContext "GET" "/"
    task {
        let! resp = app next ctx
        getBody resp.Value |> should equal "Hello World, from MiyamonY"
    }

[<Fact>]
let ``Explicitly return and HttpHandler when you wnat to access the HttpContext object`` () =
    let sayHelloWorld : HttpHandler =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            let name =
                ctx.TryGetQueryStringValue "name" |> Option.defaultValue "Giraffe"
            let greeting = sprintf "Hello World, from %s" name
            text greeting next ctx

    let app = (GET >=> route "/" >=> sayHelloWorld)

    let ctx = createContext "GET" "/"
    task {
        let! resp = app next ctx
        getBody resp.Value |> should equal "Hello World, from MiyamonY"
    }

open Microsoft.Extensions.Logging
[<Fact>]
let ``handleContext fucntion is convinience function for defining new hanlder function`` () =
    let handlerWithLogging : HttpHandler =
        handleContext(
            fun ctx ->
            let logger = ctx.GetService<ILogger>()
            logger.LogInformation("From the context")
            ctx.WriteTextAsync "test")

    TestCannotExecute ()

[<Fact>]
let ``handleContext fucntion also use async function`` () =
    let handlerWithLogging2 : HttpHandler =
        handleContext(
            fun ctx ->
            task {
            let logger = ctx.GetService<ILogger>()
            logger.LogInformation("From the context")
            return! ctx.WriteTextAsync "Done working"
        })

    TestCannotExecute()

[<Fact>]
let ``to get IDDisposable disposed disposal must be called in taks{} CE`` () =
    let doSomething handler : HttpHandler =
        fun (next: HttpFunc) (ctx:HttpContext) ->
            task {
                use __ = new MemoryStream(100)
                return! handler next ctx
            }

    TestCannotExecute()

[<Fact>]
let ``Continue senario: perform some actions for the next handler`` () =
    let setHttpHeader key value: HttpHandler =
        fun (next: HttpFunc) (ctx:HttpContext) ->
            ctx.SetHttpHeader key value
            next ctx

    let app = (GET >=> route "/" >=> (setHttpHeader "test" "abc"))

    let ctx = createContext "GET" "/"
    task {
        let! resp = app next ctx
        getHeader resp.Value "test" |> should equal "abc"
    }

[<Fact>]
let ``Return early senario: return early by errors etc. `` () =
    let earlyReturn : HttpFunc = Some >> Task.FromResult

    let checkUserIsLoggedIn : HttpHandler =
        fun (next :HttpFunc) (ctx: HttpContext) ->
            if isNotNull ctx.User && ctx.User.Identity.IsAuthenticated
            then next ctx
            else setStatusCode 401 earlyReturn ctx

    let checkUserIsLogginIn2 : HttpHandler =
        fun (next :HttpFunc) (ctx:HttpContext) ->
            task {
                if isNotNull ctx.User && ctx.User.Identity.IsAuthenticated
                then return! next ctx
                else ctx.SetStatusCode 401
                     return Some ctx
            }

    TestCannotExecute()

[<Fact>]
let ``Skip senario: skip current handler`` () =
    let skip : HttpFuncResult = Task.FromResult None

    let GET : HttpHandler =
        fun (next : HttpFunc) (ctx: HttpContext) ->
            if HttpMethods.IsGet ctx.Request.Method
            then next ctx
            else skip
    let GET2 : HttpHandler =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                if HttpMethods.IsGet ctx.Request.Method
                then return! next ctx
                else return None
            }

    TestCannotExecute()

[<Fact>]
let ``RequestProcessor:get request http Header`` () =
    let someHttpHandler: HttpHandler =
        fun (next:HttpFunc) (ctx:HttpContext) ->
            let someValue =
                match ctx.TryGetRequestHeader "X-MyOwnHeader" with
                | None -> "default value"
                | Some headerValue -> headerValue
            text someValue next ctx

    let app = (GET >=> route "/" >=> someHttpHandler)

    let ctx = createContext "GET" "/"
    task {
        let! resp = app next ctx
        getBody resp.Value |> should equal "abc"
    }


[<Fact>]
let ``RequestProcessor:set response http Header`` () =
    let someHttpHandler: HttpHandler =
        fun (next:HttpFunc) (ctx:HttpContext) ->
            ctx.SetHttpHeader "X-CustomHeader" "some value"
            next ctx

    let app = (GET >=> route "/" >=> someHttpHandler)

    let ctx = createContext "GET" "/"
    task {
        let! resp = app next ctx
        getHeader resp.Value "X-CustomHeader" |> should equal "some value"
    }

[<Fact>]
let ``RequestProcessor: set response header via setHttpHeader`` () =
    let someHttpHandler =
        fun (next :HttpFunc) (ctx:HttpContext) ->
            ctx.SetHttpHeader "X-CustomHeader" "some value"
            next ctx

    let app = (GET >=> route "/" >=> someHttpHandler)

    let ctx = createContext "GET" "/"
    task {
        let! resp = app next ctx
        getHeader resp.Value "X-CustomHeader" |> should equal "some value"
    }

[<Fact>]
let ``RequestProcessor: set status code via httpHandler`` () =
    let someHttpHandler : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            ctx.SetStatusCode 200
            next ctx

    let app = (GET >=> route "/" >=> someHttpHandler)

    let ctx = createContext "GET" "/"
    task {
        let! resp = app next ctx
        getStatusCode resp.Value |> should equal 200
    }

[<Fact>]
let ``RequestProcessor: set status code via function`` () =
    let app = (GET >=> route "/" >=> setStatusCode 200)

    let ctx = createContext "GET" "/"
    task {
        let! resp = app next ctx
        getStatusCode resp.Value |> should equal 200
    }

[<CLIMutable>]
type Person =
    {
        FirstName : string
        LastName  : string
    }

let johnDoe =
    {
        FirstName = "John"
        LastName  = "Doe"
    }

[<Fact>]
let ``RequestProcessor: status code function in sub modules`` () =
    let app = choose [
        route "/"     >=> Successful.OK "Hello World"
    ]

    let ctx = createContext "GET" "/"
    task {
        let! resp = app next ctx
        getBody resp.Value |> should equal "Helflo World"
    }

[<Fact>]
let ``Routing by routeCi is ignoring case`` () =
    let app = choose [GET >=> routeCi "/foo" >=> setStatusCode 200
                      RequestErrors.NOT_FOUND "Not Found"]

    let ctx = createContext "GET" "/FOO"

    task {
        let! resp = app next ctx
        getStatusCode resp.Value |> should equal 200
    }

[<Fact>]
let ``Routing by routex uses regexp`` () =
    let app = choose [GET >=> routex "/foo(/?)" >=> setStatusCode 200
                      RequestErrors.NOT_FOUND "Not Found"]

    let ctx = createContext "GET" "/foo/"

    task {
        let! resp = app next ctx
        getStatusCode resp.Value |> should equal 200
    }

[<Fact>]
let ``Routing by routef analyze url parameters`` () =
    let fooHandler (first, last, age) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            let s = sprintf "First: %s, Last: %s, Age: %i" first last age
            text s next ctx

    let app = GET >=> routef "/foo/%s/%s/%i" fooHandler
    let ctx = createContext "GET" "/foo/Miyamon/Y/33"

    task {
        let! resp = app next ctx
        getBody resp.Value |> should equal "First: Miyamon, Last: Y, Age: 33"
    }

[<Fact>]
let ``Routing by routeBind binds url parameters to record`` () =
    let fooHandler person : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            let s = sprintf "First: %s, Last: %s" person.FirstName person.LastName
            text s next ctx

    let app = GET >=> routeBind<Person> "/foo/{FirstName}/{LastName}" fooHandler
    let ctx = createContext "GET" "/foo/Miyamon/Y"

    task {
        let! resp = app next ctx
        getBody resp.Value |> should equal "First: Miyamon, Last: Y"
    }

[<CLIMutable>]
type Car =
    {
        Name   : string
        Make   : string
        Wheels : int
        Built  : DateTime
    }

[<Fact>]
let ``Model Binding: bind json`` () =
    let earlyReturn : HttpFunc = Some >> Task.FromResult
    let submitCar : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let! car = ctx.BindJsonAsync<Car>()
                match box car with
                | null -> return! RequestErrors.BAD_REQUEST "BadRequest" earlyReturn ctx
                | _ ->  return! Successful.OK car next ctx
            }

    let app = POST >=> route "/" >=> submitCar
    let ctx = createContext "POST" ""

    task {
        let! resp = app next ctx
        getStatusCode resp.Value |> should equal 200
    }
