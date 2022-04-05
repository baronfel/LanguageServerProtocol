namespace Ionide.LanguageServerProtocol

[<AutoOpen>]
module LspJsonConverters =
  open Microsoft.FSharp.Reflection
  open Newtonsoft.Json
  open System
  open System.Collections.Concurrent

  let inline memorise (f: 'a -> 'b) : ('a -> 'b) =
    let d = ConcurrentDictionary<'a, 'b>()
    fun key -> d.GetOrAdd(key, f)

  type ErasedUnionConverter() =
    inherit JsonConverter()

    let canConvert =
      memorise (fun t ->
        if not (FSharpType.IsUnion t) then
          false
        else
          t.BaseType.GetCustomAttributes(typedefof<Types.ErasedUnionAttribute>, false).Length > 0)

    override __.CanConvert(t) = canConvert t

    override __.WriteJson(writer, value, serializer) =
      let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
      let unionField = fields.[0]
      serializer.Serialize(writer, unionField)

    override __.ReadJson(_reader, _t, _existingValue, _serializer) = failwith "Not implemented"

  /// converter that can convert enum-style DUs
  type SingleCaseUnionConverter() =
    inherit JsonConverter()


    let canConvert =
      let allCases (t: System.Type) = FSharpType.GetUnionCases t

      memorise (fun t ->
        FSharpType.IsUnion t
        && allCases t
           |> Array.forall (fun c -> c.GetFields().Length = 0))

    override _.CanConvert t = canConvert t

    override _.WriteJson(writer: Newtonsoft.Json.JsonWriter, value: obj, serializer: Newtonsoft.Json.JsonSerializer) =
      serializer.Serialize(writer, string value)

    override _.ReadJson(reader: Newtonsoft.Json.JsonReader, t, _existingValue, serializer) =
      let caseName = string reader.Value

      match
        FSharpType.GetUnionCases(t)
        |> Array.tryFind (fun c -> c.Name.Equals(caseName, StringComparison.OrdinalIgnoreCase))
        with
      | Some caseInfo -> FSharpValue.MakeUnion(caseInfo, [||])
      | None -> failwith $"Could not create an instance of the type '%s{t.Name}' with the name '%s{caseName}'"

  type U2BoolObjectConverter() =
    inherit JsonConverter()

    let canConvert =
      memorise (fun (t: System.Type) ->
        t.IsGenericType
        && t.GetGenericTypeDefinition() = typedefof<Types.U2<_, _>>
        && t.GetGenericArguments().Length = 2
        && t.GetGenericArguments().[0] = typeof<bool>
        && not (t.GetGenericArguments().[1].IsValueType))

    override _.CanConvert t = canConvert t

    override _.WriteJson(writer, value, serializer) =
      let case, fields = FSharpValue.GetUnionFields(value, value.GetType())

      match case.Name with
      | "First" -> writer.WriteValue(value :?> bool)
      | "Second" -> serializer.Serialize(writer, fields.[0])
      | _ -> failwith $"Unrecognized case '{case.Name}' for union type '{value.GetType().FullName}'."

    override _.ReadJson(reader, t, _existingValue, serializer) =
      let cases = FSharpType.GetUnionCases(t)

      match reader.TokenType with
      | JsonToken.Boolean ->
        // 'First' side
        FSharpValue.MakeUnion(cases.[0], [| box (reader.Value :?> bool) |])
      | JsonToken.StartObject ->
        // Second side
        let value = serializer.Deserialize(reader, (t.GetGenericArguments().[1]))
        FSharpValue.MakeUnion(cases.[1], [| value |])
      | _ ->
        failwithf $"Unrecognized json TokenType '%s{string reader.TokenType}' when reading value of type '{t.FullName}'"

  type OptionConverter() =
    inherit JsonConverter()

    override __.CanConvert(t) =
      t.IsGenericType
      && t.GetGenericTypeDefinition() = typedefof<option<_>>

    override __.WriteJson(writer, value, serializer) =
      let value =
        if isNull value then
          null
        else
          let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
          fields.[0]

      serializer.Serialize(writer, value)

    override __.ReadJson(reader, t, _existingValue, serializer) =
      let innerType = t.GetGenericArguments().[0]

      let innerType =
        if innerType.IsValueType then
          (typedefof<Nullable<_>>).MakeGenericType([| innerType |])
        else
          innerType

      let value = serializer.Deserialize(reader, innerType)
      let cases = FSharpType.GetUnionCases(t)

      if isNull value then
        FSharpValue.MakeUnion(cases.[0], [||])
      else
        FSharpValue.MakeUnion(cases.[1], [| value |])