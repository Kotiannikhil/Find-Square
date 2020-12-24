#time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit"

open System
open Akka.FSharp
open Akka.Actor

type ProcessorJob = Msg of int * int * int
let system = System.create "MySystem" <| Configuration.load()
let mutable flag = false
let mutable count = 0
let numcores = 7;
type Command =
    | Testinput of string
let root x:bigint =
    let mutable start = bigint 1
    let mutable ending:bigint = x
    let mutable result = bigint -1
    while (start <= ending) do
        let mutable mid:bigint = (start + ending)/bigint 2
        if mid * mid = x then
            result <- mid
        if mid * mid < x then 
            start <- mid + bigint 1
        else 
            ending <- mid - bigint 1
    result

let workerref (mailbox : Actor<_>) =
        let rec loop() = actor {
            let! Msg(n1,n2,k) = mailbox.Receive()
            let mutable sum = bigint 0
            let mutable ans = bigint 0
            let i = 1
            for i = n1 to n2 do 
                for j = i to i + k - 1 do
                    let mutable pow = bigint j
                    sum <- sum + pown pow 2
                    ans <- root sum
                if ans <> bigint -1 then
                    printfn "%d" i
                sum <- bigint 0
            mailbox.Sender() <! Msg(-1,-1,-1)
            return! loop() }
        loop()

let bossref (mailbox : Actor<_>) =
        let rec loop() = actor {
            let! Msg(n1,n2,k) = mailbox.Receive()
            if k = -1 then
                count <- count + 1
            else    
                if n2 <= numcores then
                    let child = spawn system (sprintf "child%i" n2) workerref
                    child <! Msg(n1, n2, k)
                    return! loop()
                let subprob = n2/numcores;
                let mutable num = 0;
                while(num < numcores) do
                    if num = numcores-1 && subprob + subprob*num < n2 then
                        let child = spawn system (sprintf "child%i" num) workerref
                        child <! Msg(1+num*subprob,n2,k)
                    else
                        let child = spawn system (sprintf "child%i" num) workerref
                        child <! Msg(1+num*subprob,subprob + subprob*num,k)   
                    num<-num + 1
            return! loop()
        }
        loop()
        
let arr:string array = fsi.CommandLineArgs |> Array.tail
let n = arr.[0] |> int
let k = arr.[1] |> int
let boss = spawn system "Boss" bossref
if n <> 0 && k <> 0 then
    boss <! Msg(1,n,k)
while not flag && n <> 0 && k <> 0 do
     if count = 1 && n <= numcores then
        flag <- true
     if count = numcores then
        flag <- true