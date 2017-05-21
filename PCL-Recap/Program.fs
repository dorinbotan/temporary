open System

// Recursion means that a defined entity refers to itself in the definition (Example: the factorial function “!” on natural numbers 0! = 1, n! = n + (n − 1)!, n > 0)

// for i in 1..10 do

// if x > y then true else false

// match expr with
//     | pattern1 -> 1
//     | pattern2 -> 2
//     | _ -> 3

// (1, "MyTuple", (13L, 0))

// Output
let addXY (x : int) y =
    Console.WriteLine("{0} + {1}", x, y)
    printfn "%i + %i" x y
    x + y

// List building
let a = [for i in 1..10 -> i * i] // -> abbreviation of do yield
let b = [1..10]
let c = [0..10..100]
let d = [for i in 1..100 do if i % 3 = 0 then yield i]
let e n = [for i in 1..n -> (i, i*i)]
let f n = [for (x, y) in e n -> x + y] // List with sums of elements in tuples in a tuple list
let g = 
    [
        for a in 1..5 do
            match a with
            | 3 -> yield! [13; 14; 15]
            | _ -> yield a
    ]
let x = 1 :: [2; 3]
let y = [1] @ [2; 3]

// List processing
let add (a : int list) =
    let rec tmp ret a =
        match a with
        | head::tail -> tmp (ret + head) tail
        | _ -> ret
    tmp 0 a

// Strings
let stringToCharList (s : string) = [for c in s -> c]
let charListToString (c : char list) = c |> System.String.Concat
("Hello" + " world!").[0..4]

// List functions
List.sum [1; 2; 3]
List.rev [1; 2; 3]
List.fold (*) 1 [1; 2; 3]
List.map (fun x -> x + 1) [1; 2; 3]
List.filter (fun x -> x % 2 <> 0) [1; 2; 3]

// Higher order functions
let rec map f x =
    match x with
    | [] -> []
    | head::tail -> (f head)::(map f tail)
let pow2 = map (fun x -> x + 1)

let pclTwice f x = f (f x)
let pclQuad x = pclTwice (fun x -> x * x) x

// Fibonacci
let rec fib x =
    match x with
    | 0 -> 0
    | 1 -> 1 
    | n when n < 0 -> failwith "Negative parameter" // when - guard
    | n -> (fib (n - 1)) + (fib (n - 2))

// Factorial
let rec fac x =
    match x with
    | n when n <= 1 -> 1
    | n -> n * fac (x - 1)

let rec facTailRecursive x =
    let rec tmp x acc =
        if x <= 1 then
            acc
        else
            tmp (x - 1) (acc * x)
    tmp x 1

// Lambda (anonymous/nameless function)
(fun x -> x + 1)

// fun x y -> e shorthand for fun x -> (fun y -> e)
// let (rec) f x = ... shorthand for fun x -> (...)

// Currying - transforming a function with multiple arguments into a function with only one argument
// Transform a function with several argumenta into a function of n functions, each taking one argument
// let add x y = x + y
// add10 = add 10

// Closure - first order function containing references to variables declared outside the function and not being it's parameters
// Allows to make complicated functions from simpler ones
// let add i = List.map (fun x -> x + i)

// Pipelining (<|, |>) - helps to get rid of 'let' statements
// Values flow through the functions in the pipeline from left to right
// let (|>) x fs = fs x
// let tmp x y = (x + (y * 3)) may be written as let tmp x y = y * 3 |> (+) x

// Function composition (<<, >>)
// let (>>) f g = fun x -> g (f x)
// let tmp = (fun x -> x * x) >> (fun (x : int) -> x.ToString())
// let tmp = ((*) -1) >> (+)

// data types (Names of user defined constructors must start with Upper-case)
type lineNumber = int
type Color = Black | Blue | Green | Cyan | Red | Magenta | Yellow | White
type Flower = Tulip of Color | Rose of Color | Test of int * Color

let myFlowers =
    [
        for color in [Black; Blue; Green; Cyan; Red; Magenta; Yellow; White] do
            yield Tulip(color)
            yield Rose(color)
            for i in 1..10 do
                yield Test(i, color)
    ]

let MyDataType (x : Color) = 
    match x with
    | Black -> 1
    | _ -> 0

// Tail recursive - if and only if there is no work to be performed after a recursive call is executed
let rec add x y =
    match x with
    | 0 -> y
    | n -> 1 + add (n - 1) y

let rec add x y =
    match x with
    | 0 -> y
    | n -> add (n - 1) (y + 1)

// Mutable variables - can change their cotent
let mutable x = 5
let mutable myFun = fun x -> x <- x + 1

myFun 1

// Reference type
let x = [|for i in 1..10 -> i|]
let y = x
x.[0] <- 3
y




// ---------------------------------------------------------------------------------------------




let charToUpper c =
    match c with
    | 'a' -> 'A'
    | 'b' -> 'B'
    | 'c' -> 'C'
    | _ -> c

let stringToUpper (s : string) = [for c in s -> charToUpper c] |> System.String.Concat

let pclLength (a : _ list) =
    let rec tmp ret (a : _ list) =
        match a with
        | head::tail -> tmp (ret + 1) tail
        | _ -> ret
    tmp 0 a
pclLength [for c in "" -> c]

let takeSomeRecursive n ls =
    let rec tmp n h t =
        match n, t with
        | 0, _ -> h
        | x, head::tail -> tmp (n - 1) (h @ [head]) tail
        | _, _ -> failwith "Wrong input parameters"
    tmp n [] ls

let takeSome n (ls : _ list) = [for i in 1..n -> ls.[i - 1]]

takeSome 5 [for c in "Hello world!" -> c]

let listReverse (ls : _ list) = [for i in (List.length ls)..(-1)..1 -> ls.[i - 1]]

let isPalindrome (s : string) = [for c in s -> c] = listReverse ([for c in s -> c])

let combine l1 l2 = [for i in 1..(min (List.length l1) (List.length l2)) -> (l1.[i - 1], l2.[i - 1])]

[for c in "Hello" do yield! [c; 'a']] |> System.String.Concat // Add 'a' after every character

let rec pclFold op acc ls =
    match ls with
    | head::tail -> pclFold op (op acc head) tail
    | _ -> acc
pclFold (+) 0 [1; 2; 3; 4]

let pclFoldBack op acc ls =
    let rec tmp acc ls = 
        match ls with
        | head::tail -> tmp (op acc head) tail
        | _ -> acc
    tmp acc [for i in (List.length ls)..1 -> ls.[i - 1]]

let rec pclFoldBackFast op acc ls = // Still slower than pclFold, since it is not tailRecursive
    match ls with
    | [] -> acc
    | head:: tail -> op head (pclFoldBack op acc ls)

let pclIncList ls = [for i in ls -> i + 1]

let pclIncListRecursive ls =
    let rec tmp ret ls =
        match ls with
        | head::tail -> tmp (ret @ [head + 1]) tail
        | _ -> ret
    tmp [] ls

let pclMap op ls = [for i in ls -> op i]

let pclMapRecursive op ls =
    let rec tmp ret ls =
        match ls with
        | head::tail -> tmp (ret @ [op head]) tail
        | _ -> ret
    tmp [] ls

let pclIncListMap = pclMapRecursive (+)

let pclFilter op ls = [for i in ls do if op i then yield i]

let pclFilterRecursive op ls =
    let rec tmp ret ls =
        match ls with
        | head::tail ->
            if op head then
                tmp (ret @ [head]) tail
            else
                tmp ret tail
        | _ -> ret
    tmp [] ls

let pclEven x = x % 2 = 0

pclFilter pclEven [0; 1; 2; 3; 4; 5]

let discard n ls = 
    if n < 0 then
        failwith "Negative argument not allowed"
    else if n > (List.length ls) then
        failwith "List is too short"
    else
        [for i in n..(List.length ls - 1) -> ls.[i]]

let rec lookup p ls =
    match ls with
    | [] -> 0
    | head::tail -> if p head then 0 else (1 + lookup p tail)

let nextWS ls = lookup (fun x -> x = ' ' || x = '\n' || x = '\t') ls
let nextNWS ls = lookup (fun x -> x <> ' ' && x <> '\n' && x <> '\t') ls

let rec convertStringToWords ls =
    match ls with
    | [] -> []
    | _ -> [for i in 1..(nextWS ls) -> ls.[i - 1]] :: convertStringToWords (discard (nextNWS (discard (nextWS ls) ls)) (discard (nextWS ls) ls))

let countOccurencies c ls = List.fold (fun x y -> if y = c then x + 1 else x) 0 ls

let countNumOfVowels ls =
    [
        countOccurencies 'a' ls; 
        countOccurencies 'e' ls;
        countOccurencies 'i' ls;
        countOccurencies 'o' ls
    ]