module basicExt

#if WEBSHARPER
#nowarn "3242" 

    open WebSharper
    //open WebSharper.JavaScript
    open WebSharper.UI
    open WebSharper.UI.Client
    type on   = WebSharper.UI.Html.on
    type attr = WebSharper.UI.Html.attr
#else

    /// dummy WebSharper definition in order to avoid having to use #if WEBSHARPER all the time
    module WebSharper =
        type RpcAttribute() =
            inherit System.Attribute()
            let a = 1
        type JavaScriptAttribute(translate:bool) =
            inherit System.Attribute()
            let a = 1
            new() = JavaScriptAttribute true
        type JavaScriptExportAttribute(translate:bool) =
            inherit System.Attribute()
            let a = 1
            new() = JavaScriptExportAttribute true
        type InlineAttribute(code:string) =
            inherit System.Attribute()
            let a = 1
            new() = InlineAttribute ""
        type DirectAttribute(code:string) =
            inherit System.Attribute()
            let a = 1

    open WebSharper

    #endif

    let [<Inline>] inline swap f a b = f b a

    /// swap: for use with operators: [1..5] |> List.map (__ (/) 2)
    let [<Inline>] inline __   f a b = f b a

    /// call a function but return the input value
    /// for logging, debugging
    /// use: (5 * 8) |> tee (printfn "value = %d") |> doSomethingElse
    let [<Inline>] inline tee f v = f v ; v

    /// tee: call a function but return the input value
    /// for logging, debugging
    /// use: (5 * 8) |!> printfn "value = %d" |> doSomethingElse
    let [<Inline>] inline  (|>!) v f   = f v ; v
    let [<Inline>] inline  (>>!) g f   = g >> fun v -> f v ; v

    let inline print v = 
        match box v with
        | :? string as s -> printfn "%s" s
        | __             -> printfn "%A" v

    //#define TEE
