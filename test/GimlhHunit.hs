import Gimlh
import Test.HUnit

testParseStringWTextAssertion :: Assertion
testParseStringWTextAssertion =
    let node = ("this_is_text", TextG, Text $ "abc")
        giml = [node]
    in
      giml @=? parseString ":text: this_is_text\nabc"

testParseStringWMLTextAssertion :: Assertion
testParseStringWMLTextAssertion =
    let node = ("this_is_mtext", TextG, Text $ "abc\ndef")
        giml = [node]
    in
      giml @=? parseString ":text: this_is_mtext\nabc\ndef"

testParseStringWNumAssertion :: Assertion
testParseStringWNumAssertion =
    let node = ("this_is_num", NumberG, Number $ 123)
        giml = [node]
    in
      giml @=? parseString ":num: this_is_num\n123"

testParseStringWFloatAssertion :: Assertion
testParseStringWFloatAssertion =
    let node = ("this_is_float", FloatG, Float $ 123.5)
        giml = [node]
    in
      giml @=? parseString ":num: this_is_float\n123.5"

testParseStringWListAssertion :: Assertion
testParseStringWListAssertion =
    let node = ("this_is_list", ListG, List $ ["a", "b", "c"])
        giml = [node]
    in
      giml @=? parseString ":list: this_is_list\na, b, c"

testParseStringWVListAssertion :: Assertion
testParseStringWVListAssertion =
    let node = ("this_is_vlist", ListG, List $ ["a", "b", "c"])
        giml = [node]
    in
      giml @=? parseString ":list: this_is_vlist\n- a\n- b\n- c"

testParseStringWListWCommaAtEndAssertion :: Assertion
testParseStringWListWCommaAtEndAssertion =
    let node = ("this_is_list", ListG, List $ ["a", "b", "c"])
        giml = [node]
    in
      giml @=? parseString ":list: this_is_list\na, b, c,"

testParseStringWListWNLAtEndAssertion :: Assertion
testParseStringWListWNLAtEndAssertion =
    let node = ("this_is_list", ListG, List $ ["a", "b", "c"])
        giml = [node]
    in
      giml @=? parseString ":list: this_is_list\na, b, c\n"

testParseStringWNodesAssertion :: Assertion
testParseStringWNodesAssertion =
    let node1 = ("this_is_list", ListG, List $ ["a", "b", "c"])
        node2 = ("this_is_text", TextG, Text $ "abc")
        node3 = ("this_is_num", NumberG, Number $ 123)
        giml = [node1, node2, node3]
    in
      giml @=? parseString ":list: this_is_list\na, b, c,\n:text: this_is_text\nabc\n:num: this_is_num\n123"

testAll = do
    testParseStringWTextAssertion
    testParseStringWListAssertion
    testParseStringWListWCommaAtEndAssertion
    testParseStringWNumAssertion
    testParseStringWFloatAssertion
    testParseStringWMLTextAssertion
    testParseStringWVListAssertion
    testParseStringWNodesAssertion
