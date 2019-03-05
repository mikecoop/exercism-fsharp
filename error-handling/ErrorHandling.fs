module ErrorHandling

let handleErrorByThrowingException() = failwith ""

let handleErrorByReturningOption (input:string) =
    match System.Int32.TryParse input with
    | (true, i) -> Some i
    | (false, _) -> None

let handleErrorByReturningResult (input:string) =
    match System.Int32.TryParse input with
    | (true, i) -> Ok i
    | (false, _) -> Error "Could not convert input to integer"

let bind switchFunction twoTrackInput = twoTrackInput |> Result.bind switchFunction

let cleanupDisposablesWhenThrowingException resource =
    using (resource) (fun _ -> failwith "Exception")
        