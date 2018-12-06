import Lexer
import Test.HUnit

test1 = TestCase (assertEqual "for isNum (non int)", (False) (isNum "12g4"))
test2 = TestCase (assertEqual "for isNUm (int)", (True) (isNum "1234"))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
