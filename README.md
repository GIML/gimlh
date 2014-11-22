# gimlh

Haskell parser for GIML.

## Install

Working on it

You can just clone and import module into your haskell code

## Usage

There are two methods: `parseString` and `parseFile`

`parseString` will parse `GIML` from pure string.

`parseFile` accepts `FilePath` and parses it into `IO Giml`

`Giml` - is a type which contains list of `GimlNode`s

`GimlNode` - is a tuple `(String, GimlType, GimlVal)`

`GimlVal` - can be one of four types: `Text String | List [String] | Number Integer | Float Double`

`GimlType` - types for `GimlVal` stored in `GimlNode`. Can be `TextG | ListG | NumberG | FloatG`
