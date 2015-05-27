module Database.SQL.SQLConverter.Types (
    FieldType,
    FieldName,
    TableName,
	RelationField,
	Field (RegularField,Key,Relation),
	Table (Table),
    Corresponds(Corresponds),
    Scheme,
    TableOperations (tName,tBody)
	
) where
import qualified Data.Text as T 
import qualified Data.Set as S


type FieldType = T.Text --используется только в сыром представлении. RegularField|Key|Relation
type DataType = T.Text --тип данных поля
type FieldName = T.Text 
type TableName = T.Text 
type RelationField = T.Text --куда поле указывает


data Field = RegularField FieldName DataType 
	| Key FieldName DataType 
	| Relation FieldName DataType TableName RelationField 
        deriving (Eq,Ord)

data Table = Table TableName (S.Set Field) deriving (Ord,Eq)


class TableOperations a where
    (+++) :: a -> a -> a --суммировать поля
    tName :: a -> TableName
    tBody :: a -> S.Set Field

instance TableOperations Table where
    tName (Table name _) = name
    tBody (Table _ body) = body
    (+++) a  b  
        | aName /= bName = a --если имена таблиц разные то возвращаем первую таблицу
        | aName == bName = Table aName $ S.union (tBody a)  (tBody b) --если одинаковые - то новую с суммой полей
        where
            aName = tName a
            bName = tName b
 
    

type Scheme = S.Set Table --типа база

--при конвертации поля друг другу соответствуют. 
--для отличия 
type CorrespondFrom = (TableName, FieldName)
type CorrespondTo  = (TableName, FieldName)

data Corresponds = Corresponds [(CorrespondTo, CorrespondFrom)]




