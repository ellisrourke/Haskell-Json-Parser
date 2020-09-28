{--
    - JSON Data Types
    - Ellis Rourke
--}

data Value =  
      String
      | Integer
      | Double
      | Object
    deriving Show
      
type Object = [(String,[Value])]

