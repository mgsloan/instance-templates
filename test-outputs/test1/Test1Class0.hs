module Test1Class0 where
import NewPrelude
 
class Foo a where
         
        foo :: a
 
class Bar a where
         
        bar :: a
[deriving| 
class Foo a where
         
        instance Foo a
|]
[deriving| 
class Bar a where
         
        instance Bar a
|]
