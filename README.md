# gimlh

Haskell parser for GIML.

## Install

Working on it

You can just clone and import module into your haskell code

## Usage

There are three methods: `parseString`, `parseFile` and `simplifyGiml`

`parseString` will parse `GIML` from pure string.

`parseFile` accepts `FilePath` and parses it into `IO Giml`

`simplifyGiml` translates parsed `GIML` in simply form `SimplyGiml`

`Giml` - is a type which contains list of `GimlNode`s

`SimplyGiml` - is a list of tuples `(String, GimlVal)` == `(key, value)`

`GimlNode` - is a tuple `(String, GimlType, GimlVal)`

`GimlVal` - can be one of four types: `Text String | List [String] | Number Integer | Float Double`

`GimlType` - types for `GimlVal` stored in `GimlNode`. Can be `TextG | ListG | NumberG | FloatG`
