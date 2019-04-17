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
        let newestToOldest = value :: buffer.Items
        { buffer with Items = newestToOldest }

let forceWrite value buffer =
    if bufferIsFull buffer then
        let oldestToNewest = (buffer.Items |> List.rev) |> List.tail
        let newestToOldest = (value :: (oldestToNewest |> List.rev))
        { buffer with Items = newestToOldest }
    else
        write value buffer

let read buffer =
    match buffer.Items |> List.rev with
    | [ ] ->
        failwith "Buffer is empty"
    | oldest :: oldestToNewest ->
        oldest, { buffer with Items = oldestToNewest |> List.rev }