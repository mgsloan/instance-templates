module Test1 where
import NewPrelude
 
deriver Instance [d|foo = 0|] (deriver Instance [d|bar = 0.0|])