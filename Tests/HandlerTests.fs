module HandlerTests

open System
open System.IO
open System.Text
open System.Threading.Tasks

open Xunit
open FsUnit
open NSubstitute
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks
open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.AspNetCore.TestHost

open Main

let createContext (method_ : string) (path : string) =
    let ctx = Substitute.For<HttpContext>()
    ctx.Request.Path.Returns(PathString(path)) |> ignore
    ctx.Request.Method.ReturnsForAnyArgs method_ |> ignore
    ctx

let getBody (ctx : HttpContext) =
    ctx.Response.Body.Position <- 0L
    use reader = new StreamReader(ctx.Response.Body, Encoding.UTF8)
    reader.ReadToEnd()

let next = Some >> Task.FromResult

[<Fact>]
let ``Response Json`` () =

    let app = GET >=> route "/" >=> jsonHandler
    let ctx = createContext "GET" "/"

    task {
        let! result = app next ctx
        getBody result.Value |> should equal """{"Date":"2020/10/1:00:00:00", "Name":"test"}"""
    }
