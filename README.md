# gimlh

Haskell parser for GIML.

## Install

```bash
cabal install gimlh
```

## Usage

```haskell
import Gimlh

getGiml :: FilePath -> IO SimpleGiml
getGiml path = do
  giml <- parseFile path
  let simplified = simplifyGiml giml
  return simplifyGiml
```

## Documentation

[haddock](http://hackage.haskell.org/package/gimlh-0.1.0.0/docs/Gimlh.html)

There are three methods: `parseString`, `parseFile` and `simplifyGiml`

`parseString` will parse `GIML` from pure string.

`parseFile` accepts `FilePath` and parses it into `IO Giml`

`simplifyGiml` translates parsed `GIML` in simply form `SimplyGiml`

`Giml` - is a type which contains list of `GimlNode`s

`SimplyGiml` - is a list of tuples `(String, GimlVal)` == `(key, value)`

`GimlNode` - is a tuple `(String, GimlType, GimlVal)`

`GimlVal` - can be one of four types: `Text String | List [String] | Number Integer | Float Double`

`GimlType` - types for `GimlVal` stored in `GimlNode`. Can be `TextG | ListG | NumberG | FloatG`
