namespace Mana

open System.Collections
open System.Collections.Generic
open Mana.Cont

type Channel<'T> = {
    mutable closed: bool
    capacity: int
    buffer: Queue<'T>
    sendQueue: Queue<(unit -> 'T) * 'T>
    recvQueue: Queue<'T -> 'T>
} with

    static member Create size = {
        closed = false
        capacity = size
        buffer = Queue<'T>()
        sendQueue = Queue<(unit -> 'T) * 'T>()
        recvQueue = Queue<'T -> 'T>()
    }

    member this.IsFull() = this.buffer.Count = this.capacity

    member this.BufDequeue() = Queue.tryDequeue this.buffer

    member this.BufEnqueue v = this.buffer.Enqueue v

    member this.RecvDequeue() = Queue.tryDequeue this.recvQueue
    member this.RecvEnqueue co = this.recvQueue.Enqueue co

    member this.SendDequeue() = Queue.tryDequeue this.sendQueue
    member this.SendEnqueue co v = this.sendQueue.Enqueue(co, v)

type Coroutine<'T> = unit -> 'T

type Context<'T>() =
    let queue = Queue<Coroutine<'T>>()
    member this.ScheduleCoroutine(co: Coroutine<'T>) = queue.Enqueue co

    member this.GetNextCoroutine() =
        match queue.TryDequeue() with
        | true, co -> Some co
        | _ -> None

    member this.RunNextCoroutine() =
        match this.GetNextCoroutine() with
        | Some co -> co ()
        | None -> failwith "no more coroutine to run"

    member this.CloseChannel(channel: Channel<'T>) =
        channel.closed <- true
        todo "Do something about waiting receivers?"
