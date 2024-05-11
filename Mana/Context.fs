namespace Mana

open System.Collections
open System.Collections.Generic
open Mana.Cont

type Channel<'T> = {
    mutable closed: bool
    capacity: int
    buffer: Queue<'T>
    sendQueue: Queue<('T -> 'T) * 'T>
    recvQueue: Queue<'T -> 'T>
} with

    static member Create size = {
        closed = false
        capacity = size
        buffer = Queue<'T>()
        sendQueue = Queue<('T -> 'T) * 'T>()
        recvQueue = Queue<'T -> 'T>()
    }

    member this.IsFull() = this.buffer.Count = this.capacity

    member this.CanRecv() = this.recvQueue.Count < 4
    member this.CanSend() = this.sendQueue.Count < 4

    member this.BufDequeue() = Queue.tryDequeue this.buffer

    member this.BufEnqueue v = this.buffer.Enqueue v

    member this.RecvDequeue() = Queue.tryDequeue this.recvQueue
    member this.RecvEnqueue co = this.recvQueue.Enqueue co

    member this.SendDequeue() = Queue.tryDequeue this.sendQueue
    member this.SendEnqueue co v = this.sendQueue.Enqueue(co, v)

and Coroutine<'T> = ('T -> 'T) -> 'T

type Context<'T>() =
    let queue = Queue<Coroutine<'T>>()

    // This can be used to implement
    // - try catch : raise
    // - seq : yield
    // - proc : early return
    let effectHandlers = Stack<unit>()

    member this.ScheduleCoroutine(co: Coroutine<'T>) = queue.Enqueue co

    member this.GetNextCoroutine() =
        match queue.TryDequeue() with
        | true, co -> Some co
        | _ -> None

    member this.RunNextCoroutine k =
        match this.GetNextCoroutine() with
        | Some co -> co k
        | None -> failwith "no more coroutine to run"

    member this.CloseChannel(channel: Channel<'T>) =
        channel.closed <- true
        todo "Do something about waiting receivers?"
