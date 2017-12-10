module Tests

open Expecto
open FsHtmlText

[<Tests>]
let tests =
  testList "html tests" [
    testCase "should parse very big file" <| fun _ ->
      let text = String.replicate 10000 "aaa"
      let html = sprintf "<htnl>%s</htnl>" text
      let parsed = FsHtmlText.parse html
      Expect.equal parsed text "Parsed test should be equal to input."

    testCase "should fail" <| fun _ ->
      let subject = false
      Expect.isTrue subject "I should fail because the subject is false."
  ]