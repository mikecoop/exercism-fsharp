module BankAccount

type OpenAccount = { mutable Balance: decimal }

type Account =
    | Closed
    | Open of OpenAccount

let mkBankAccount() = Closed

let openAccount account =
    match account with
    | Closed -> Open { Balance = 0.0m }
    | Open _ -> failwith "Account is already open"

let closeAccount account =
    match account with
    | Open _ -> Closed
    | Closed -> failwith "Account is already closed"

let getBalance account =
    match account with
    | Open openAccount -> Some openAccount.Balance
    | Closed -> None

let updateBalance change account =
    match account with
    | Open openAccount ->
        lock (openAccount) (fun _ ->
            openAccount.Balance <- openAccount.Balance + change
            Open openAccount)
    | Closed -> failwith "Account is closed"