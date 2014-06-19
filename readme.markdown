JsonFSharp - Another json parser for F#
=======================================

This library provides a json parser that specifically targets the F#
programming language. 

This is currently in alpha stage. This library was created because I needed the
functionality for a different project, and I wasn't happy with the
alternatives. The current state of this project does most of what I need right
now, but if you find this to be helpful and require new features, let me know,
and I will try to get the time to add that.

Also note that I haven't given the grouping of functions and types into modules
much thought, so the placement and naming of functions is subject to change.

Usage
-----

The parser specifically targets F# by generating discriminated union types:

```F#
type JsonValue =
    | JsonString of string
    | JsonNumber of double
    | JsonBool of bool
    | JsonNull
    | JsonArray of JsonValue list
    | JsonObject of Map<string,JsonValue>
```

The parser can handle either string or stream input

```F#
let resultFromString =
    stringInput 
    |> JsonInput.fromString 
    |> parse

let resultFromStream =
    stream
    |> JsonInput.fromStream
    |> parse
```

The result is a Result<'TSuccess,'TFailure>, so the parser follows the railway
oriented style: http://fsharpforfunandprofit.com/posts/recipe-part2/

Converting to F# record types
-----------------------------

The parser can also convert the parsed JsonValue to an F# record type. This is
currently not complete.

```F#
type Person = {
    firstName: string;
    lastName: string;
    age: int }

"""{
    "firstName": "John",
    "lastName": "Doe",
    "age": 42 }"""
|> JsonInput.FromString
|> parse
>>= toInstance<Person>
```

This will instantiate a Person record from the json. (The >>= operator unwraps
the Result<,> type from the parse function, and calls toInstance if the parsing
succeeded)

Again, this returns a Result<,> type

If the json object does not contain values for all fields, or if some values
cannot be converted to the correct type (e.g. if age had been a string in the
json), a Failure will be returned. No Failure is returned if there are 'too
many' values in the json, as long as all the record members can be evaluated.

The record construction supports records (no classes)\*, arrays, option values,
F# tuples from json arrays, maps of strings, and simple types. But no
discriminated unions. 

Map deserialization example

```fsharp
type FooType = { foo : int; }
type ParentTypeWithMap = {
        children: Map<string,FooType>
    }
"""{ "children": {
       "a" : { "foo": 42 },
       "b" : { "foo": 43 } } }"""
|> JsonInput.FromString
|> parse
>>= toInstance<ParentTypeWithMap>
```

It serializes the children collection into a map with the keys "a" and "b".

Tuple deserialization exampel

```fsharp
type FooType = int * int * string
type ParentTypeWithTupleArray = {
        Children: FooType list
    }
"""{ "children": [
	[1,2,"foo"],
	[3,4,"bar"]] """
|> JsonInput.FromString
|> parse
>>= toInstance<ParentTypeWithTupleArray>
```

\* The record construction simply uses reflection to find a single constructor
  and retrieve all the required parameters from the json data. So if you have a
class with a single argument, and all necessary data is passed through the
constructor, I think that would work as well. But it's not supported, and not
a priority.
