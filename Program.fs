open System.IO
open System
open System.Text


[<EntryPoint>]
let main argv =
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let isPrime n =
        let sqrt' = (float >> sqrt >> int) n
        [ 2 .. sqrt' ]
        |> List.forall (fun x -> n % x <> 0)

    let allPrimes =
        let rec allPrimes' n =
            seq {
                if isPrime n then
                    yield n
                yield! allPrimes' (n+1)
            }
        allPrimes' 2

    let primes = allPrimes |> Seq.take 80 |> List.ofSeq
    let createLookup alphabet =
        let ignoredChars = ['.';':';';']
        let nchars = alphabet |> Seq.length
        Seq.zip alphabet (primes |> Seq.take nchars)
        |> Seq.append (ignoredChars |> Seq.map (fun c -> (c,1)))
        |> dict

    let aplh = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'R'; 'S'; 'Š'; 'Z'; 'Ž'; 'T'; 'U'; 'V'; 'Õ'; 'Ä'; 'Ö'; 'Ü'; 'F'; 'Š'; 'Z'; 'Ž'; 'Y'; 'Q'; 'W'; 'X' ]
    let alphLower = aplh |> List.map (fun z -> (System.Char.ToLower(z)))
    let all = List.append aplh alphLower |> List.append [' '; '?'; '-'; 'õ'; '\''; '!'; 'é']

    let lookup = all |> createLookup

    let computeHash (s:string) =
        let folder acc c =
            acc * (bigint (lookup.[c]))
        s |> Seq.fold folder 1I

    let findAnas word1 (array: string []) =
        let targetHash           = computeHash word1
        array |> Array.filter (fun x -> targetHash = computeHash x)

    let array = File.ReadAllLines(argv.[1], Encoding.GetEncoding(1257))
    let answer = (findAnas argv.[0] array)
    let (^) l r = sprintf "%s, %s" l r
    stopWatch.Stop()
    printfn "%A" ((stopWatch.Elapsed.TotalMilliseconds * (float 1000000)).ToString() ^ (answer |> Array.reduce (fun acc (value: string) -> acc ^ value)))
    1