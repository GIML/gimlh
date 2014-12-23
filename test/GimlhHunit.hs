import Gimlh
import Test.HUnit

testParseStringAssertion :: Assertion
testParseStringAssertion =
    let node = ("this_is_text", TextG, Text $ "abc")
        giml = [node]
    in
      giml @=? parseString ":text: this_is_text\nabc"
