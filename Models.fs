module Models

open System
open System.Linq
open Microsoft.EntityFrameworkCore
open FSharp.Control.Tasks.V2.ContextInsensitive

type Type =
    | Foods = 0
    | Commodity = 1
    | Etc = 2

type ID = int

[<CLIMutable>]
type Account =
    {
        ID: ID
        CreatedDate: DateTime
        UpdatedDate: DateTime

        Name: string
        Type: Type
        Amount: int
        UsedDate: DateTime

    }

    static member Create name type_ usedDate amount =
        let now = DateTime.Now
        { ID=0; CreatedDate=now; UpdatedDate=now; Name=name; Type=type_; UsedDate = usedDate; Amount=amount }

type AccountContext(options : DbContextOptions<AccountContext>) =
    inherit DbContext(options)

    override this.OnModelCreating(build: ModelBuilder) =
        ()

    [<DefaultValue>]
    val mutable accounts:DbSet<Account>

    member this.Accounts
        with get() = this.accounts
        and set v = this.accounts <- v

let getAccount (context:AccountContext) (id:ID) =
    task {
        let! found = context.Accounts.FindAsync(id)
        return (match box found with
                | null ->  (Error (sprintf "account(id=%d) not found" id))
                | _ ->  Ok found)
    }

let getAccounts (context:AccountContext) =
    context.Accounts

let getAccountsInterval (context:AccountContext) (from:DateTime) (to_:DateTime) =
    query {
        for account in context.Accounts do
        where (from <= account.UsedDate && account.UsedDate < to_)
        select account
    }

let addAccount (context:AccountContext) (entity:Account) =
    task {
        let! _ =  context.Accounts.AddAsync(entity)

        let! result = context.SaveChangesAsync true
        return if result >= 1 then Ok entity else Error "database error"
    }

let updateAccount (context:AccountContext) (id:ID) (entity:Account) =
    task {
        let! found = context.Accounts.FindAsync(id)
        match box found with
        | null -> return (Error (sprintf "account(id=%d) not found" id))
        | _ ->
            let updated = {found with Name=entity.Name; Type=entity.Type;Amount=entity.Amount; UpdatedDate=DateTime.Now}
            context.Entry(found).CurrentValues.SetValues(updated)

            let! result = context.SaveChangesAsync true
            return if result >= 1 then Ok entity else Error "database error"
    }

let deleteAccount (context:AccountContext) (id:ID) =
    task {
        let! found = context.Accounts.FindAsync(id)
        match box found with
        | null -> return (Error (sprintf "account(id=%d) not found" id))
        | _ ->
            let _ = context.Accounts.Remove(found)

            let! result = context.SaveChangesAsync true
            return if result >= 1 then Ok found else Error "datbase error"
    }
