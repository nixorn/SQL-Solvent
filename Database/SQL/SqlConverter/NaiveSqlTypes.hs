module Database.SQL.SQLConverter.NaiveSqlTypes (

	
) where
import Database.SQL.SQLConverter.Types
import Data.Text as T
--mssql

--table, subquery




data ClauseOp = Much    | -- >
                Less    | -- <
                Equ     | -- =
                NoEqu   | -- !=
                MEqu    | -- >=
                LEqu      -- <=



--like t1.c1 = t2.c2   
data Clause     = Clause ((TableName, FieldName), ClauseOp, (TableName, FieldName)) 



data Where      = Where      [Clause]
data From       = From       TableName
data Join       = LeftJoin | InnerJoin
data LeftJoin   = LeftJoin   TableName [Clause]
data InnerJoin  = InnerJoin  TableName [Clause]

data Select     = Select [(TableName, FieldName)] From  [Join] Where
data InsertInto = InsertInto TableName [FieldName] Select
data Update     = Update TableName [FieldName] 

instance Show Select where
    show (Select fields from joins whr ) = 
        let showList a b = show a ++ show b
        in "SELECT " ++
            foldl showList fields ++
            show from ++
            foldl showList joins ++
            show whr

--data Create     = 
--data Drop       = 