module Database.SQL.SQLConverter.NaiveSqlTypes (

	
) where
import Data.Text as T
--mssql

--table, subquery
data Source     = Source T.Text

data SimpleField     = SimpleField  T.Text

--@a
data Variable   = 



data ClauseOp = Much    | -- >
                Less    | -- <
                Equ     | -- =
                NoEqual | -- !=
                MEqual  | -- >=
                LEqual    -- <=

data ClauseLog = And |
                 Or 
                 
            
            
--like t1.c1 = t2.c2   
data Clause     = Clause ((Source, SimpleField), ClauseOp, (Source, SimpleField)) |
                  Clause ((Source, SimpleField), ClauseOp, (Source, SimpleField)) ClauseLog 

data Where      = Where     [Clause]
data LeftJoin   = LeftJoin  Source [Clause]
data InnerJoin  = InnerJoin Source [Clause]

data Select     = Select Source

data Insert     =
data Update     =
data Create     =
data Drop       =