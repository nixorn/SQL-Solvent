module Database.SQL.SQLConverter.Types (
    FieldType,
    FieldName,
    TableName,
	RelationField,
	Field (Regular, Key, Relation),
	Table (Table),
    Scheme,
    TableOperations (tName,tBody),
    RelationInGraph
	
) where
import qualified Data.Text as T 
import qualified Data.Set as S


type FieldType = T.Text --используется только в сыром представлении. RegularField|Key|Relation
type DataType = T.Text --тип данных поля
type FieldName = T.Text 
type TableName = T.Text 
type RelationField = T.Text --куда поле указывает


data Field = Regular FieldName DataType 
	| Key FieldName DataType 
	| Relation FieldName DataType TableName RelationField 
        deriving (Eq,Ord,Show)
 

data Table = Table TableName (S.Set Field) deriving (Ord,Eq,Show)


class TableOperations a where
    tName :: a -> TableName
    tBody :: a -> S.Set Field
    

instance TableOperations Table where
    tName (Table name _) = name
    tBody (Table _ body) = body

 
    

type Scheme = S.Set Table --типа база

type RelationInGraph = ((TableName, FieldName),(TableName, FieldName))



