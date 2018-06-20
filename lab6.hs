import Test.HUnit

-- Exercise 1

testSum = TestCase $ assertEqual "10 + 5 = 15" 15 (10 + 5)
 
testProd = TestCase $ assertEqual "10 * 15" 150 (10 * 15)
 
testPred = TestCase $ assertBool "10 > 5" (10 > 5)
 
testFailure = TestCase $ assertEqual "It will fail 10 + 2 = 15" (10 + 2) 15  
 
testlist = TestList [ "10 + 5 = 15" ~: 15 ~=? (10 + 5),
                      "10 * 15" ~: 150 ~=? (10 * 15),
                      TestCase $ (10 > 5) @? "10 > 5",
                      "It will fail 10 + 2 = 15" ~: (10 + 2) ~?= 15 
                    ]

-- Exercise 2

reverseWords :: String -> String  
reverseWords = unwords . map reverse . words

testlist2 = TestList [ "Empty string" ~: "" ~=? reverseWords "",
                      "Capital letters" ~: "nomyzS" ~=? reverseWords "Szymon",
                      "Spacing" ~: "Ala ma kota" ~=? reverseWords "atok am alA"
                    ]

main :: IO ()
main = do
  runTestTT testlist
  runTestTT testlist2
  return ()