module CircularBuffer

type CircularBuffer<'a> =
    { Size: int
      Items: 'a list }

let bufferIsFull buffer =
    buffer.Size = List.length buffer.Items

let mkCircularBuffer size =
    { Size = size; Items = List.empty }

let clear buffer =
    { buffer with Items = List.empty }

let write value buffer =
    if bufferIsFull buffer then
        failwith "Buffer is full"
    else
        { buffer with Items = value :: buffer.Items}

let forceWrite value buffer =
    write value buffer

let read buffer =
    match buffer.Items |> List.rev with
    | [ ] ->
        failwith "Buffer is empty"
    | head :: rest ->
        head, { buffer with Items = rest |> List.rev }