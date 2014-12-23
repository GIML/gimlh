import Gimlh
import Test.HUnit

testParseStringHU :: Test
testParseStringHU =
    let node = ("this_is_text", TextG, Text $ "abc")
        giml = [node]
    in
      "Parse string" ~: giml @=? parseString ":text: this_is_text\nabc"
