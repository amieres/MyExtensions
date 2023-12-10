namespace myExtensions

type System.String with
    member this.Substring2(from, n) = 
        if   n    <= 0           then ""
        elif from >= this.Length then ""
        elif from <  0           then this.Substring2(0, n + from)
        else this.Substring(from, min n (this.Length - from))
    member this.Left             n  = if n < 0 
                                      then this.Substring2(0, this.Length + n)
                                      else this.Substring2(0, n              )
    member this.Right            n  = this.Substring2(max 0 (this.Length - n), this.Length)
    //member this.toUnderscore        = this |> Seq.mapi(fun i c -> if i > 0 && System.Char.IsUpper(c) then [ '_' ; c ] else [ c ])  |> Seq.collect id |> Seq.toArray |> System.String

module String =
    let splitByChar (c: char) (s: string) = s.Split c
    let splitInTwoO spl txt = 
        let i = (txt:string).IndexOf (spl:string)
        if  i = -1 then None else
        (txt.Left(i), txt.Substring (i + spl.Length) )
        |> Some
    let delimitedO  op cl txt =
        splitInTwoO op txt
        |> Option.bind(fun (bef, sec) ->
            splitInTwoO cl sec
            |> Option.map(fun (mid, aft) -> bef, mid, aft)
        )
    let contains (sub:string)  (whole: string) = whole.Contains sub
    let trim                  (s: string) = s.Trim()
    let left  n (s:string) = s.Left  n
    let right n (s:string) = s.Right n
    let append     (a: string)(b: string) =  a + b
    let skipFirstLine (txt:string) = txt.IndexOf '\n' |> fun i -> if i < 0 then "" else txt.[i + 1..]
    let unindent (s:string) =
        let lines = s.Split '\n'
        let n     = lines 
                    |> Seq.tryFind (fun l -> l.Trim() <> "")
                    |> Option.defaultValue ""
                    |> Seq.tryFindIndex ((<>) ' ') 
                    |> Option.defaultValue 0
        lines 
        |> Seq.map    (fun l -> if l.Length <= n then "" else l.Substring n)
        |> Seq.filter (fun s -> s.StartsWith "# 1 " |> not)
    let indent n (s:string) =
        s.Split '\n'
        |> Seq.map ((+) (String.replicate n " "))
    let unindentStr = unindent >> String.concat "\n"
    let indentStr i = indent i >> String.concat "\n" 
    let skipLastLine =
           splitByChar '\n' 
        >> fun s -> s.[0 .. (max 0 (s.Length - 2)) ]
        >> String.concat "\n"
    let (|StartsWith|_|) (start:string) (s:string) = if s.StartsWith start then Some s.[start.Length..                          ] else None
    let (|EndsWith  |_|) (ends :string) (s:string) = if s.EndsWith   ends  then Some s.[0           ..s.Length - ends.Length - 1] else None
    let (|WhiteSpace|_|) (s:string) = if s |> Seq.exists (System.Char.IsWhiteSpace >> not) then None else Some()
    
    let thousands n =
        let v = (if n < 0 then -n else n).ToString()
        let r = v.Length % 3 
        let s = if r = 0 then 3 else r
        [   yield v.[0.. s - 1]
            for i in 0..(v.Length - s)/ 3 - 1 do
                yield v.[i * 3 + s .. i * 3 + s + 2]
        ] 
        |> String.concat ","
        |> fun s -> if n < 0 then "-" + s else s

