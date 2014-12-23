import Gimlh
import Test.HUnit

testParseStringWTextAssertion :: Assertion
testParseStringWTextAssertion =
    let node = ("this_is_text", TextG, Text $ "abc")
        giml = [node]
    in
      giml @=? parseString ":text: this_is_text\nabc"

testParseStringWNumAssertion :: Assertion
testParseStringWNumAssertion =
    let node = ("this_is_num", NumberG, Number $ 123)
        giml = [node]
    in
      giml @=? parseString ":num: this_is_num\n123"

testParseStringWListAssertion :: Assertion
testParseStringWListAssertion =
    let node = ("this_is_list", ListG, List $ ["a", "b", "c"])
        giml = [node]
    in
      giml @=? parseString ":list: this_is_list\na, b, c"

testParseStringWListWCommaAtEndAssertion :: Assertion
testParseStringWListWCommaAtEndAssertion =
    let node = ("this_is_list", ListG, List $ ["a", "b", "c"])
        giml = [node]
    in
      giml @=? parseString ":list: this_is_list\na, b, c,"

testAll = do
    testParseStringWTextAssertion
    testParseStringWListAssertion
    testParseStringWListWCommaAtEndAssertion
    testParseStringWNumAssertion
