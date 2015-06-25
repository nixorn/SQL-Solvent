{- Цель - сделать генерацию минимального подмножества sql, через которое можно выразить все операции-}

module Database.SQL.SQLSolvent.NaiveSqlTypes (
        
    Ariphm(Plus, Minus, Multi, Div)     
    ,ClauseOp(Much, Less, Equ, NoEqu,MEqu, LEqu)
    ,Logic(And, Or)
    ,Clause(Clause) 
    ,Where      (Where)
    ,From       (From)
    ,Join       (LeftJoin, InnerJoin)
    ,SelectList (SelectList)
    ,Select     (Select)

) where
import Database.SQL.SQLSolvent.Types
import Data.Text as T
import Data.List as L
--mssql

--table, subquery


data Ariphm   = Plus  | -- +
                Minus | -- -
                Multi | -- *
                Div     -- /

data ClauseOp = Much    | -- >
                Less    | -- <
                Equ     | -- =
                NoEqu   | -- !=
                MEqu    | -- >=
                LEqu      -- <=
                
instance Show ClauseOp where
    show Much   = " > "
    show Less   = " < "
    show Equ    = " = " 
    show NoEqu  = " != "
    show MEqu   = " >= "
    show LEqu   = " <= "

data Logic  =   And |
                Or    



--like t1.c1 = t2.c2   
data Clause     = Clause ((TableName, FieldName), ClauseOp, (TableName, FieldName)) 
instance Show Clause where
    show (Clause ((t1, f1), op, (t2, f2))) = show t1 ++ "." ++ show f1 ++ show op ++ show t2 ++ show f2
    
data Where      = Where      [Clause]
instance Show Where where
    show (Where clauses) = " WHERE " ++ L.foldl1 (++) (fmap show clauses)
    
data From       = From       TableName
instance Show From where
    show (From t) = " FROM " ++ T.unpack t

data Join       = LeftJoin TableName [Clause]  | InnerJoin TableName [Clause]
instance Show Join where
    show (LeftJoin  t clauses) = "LEFT JOIN "  ++ T.unpack t ++ " on " ++ L.foldl1 (++) (fmap show clauses) 
    show (InnerJoin t clauses) = "INNER JOIN " ++ T.unpack t ++ " on " ++ L.foldl1 (++) (fmap show clauses) 




data SelectList = SelectList [(TableName, FieldName)]
instance Show SelectList where
    show (SelectList fields) = 
        let showL (a,b) =  T.append (T.append a $ T.append (T.pack ".") b) $ T.pack ", "
        in  T.unpack $ L.foldl1 (T.append)   (fmap showL fields)
    
    
    
data Select = Select SelectList From  [Join] Where
instance Show Select where
    show (Select fields from joins whr ) = 
        let 
        in  "SELECT " ++
            (show fields) ++
            (show from) ++
            (L.concat $ fmap show joins) ++
            (show whr)

--data InsertInto = InsertInto TableName [FieldName] Select
--data Update     = Update TableName [FieldName] 
--data Create     = 
--data Drop       = 
