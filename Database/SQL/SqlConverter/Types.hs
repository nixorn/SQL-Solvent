module Database.SQL.SQLConverter.Types (
    FieldType,
    FieldName,
	RelationField,
	Field (RegularField,Key,Relation),
	Table (Table),
    Corresponds(Corresponds),
    Scheme
    
	
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
	| Relation FieldName DataType TableName RelationField deriving Eq

data Table = Table TableName (S.Set Field)


class TableOperations Table where
    (+++) :: Table -> Table -> Table --суммировать поля
    tName :: Table -> TableName
    tBody :: Table -> S.Set Field

instance TableOperations Table where
    tName (Table name _) = name
    tBody (Table _ body) = body
    (+++) a  b  
        | aName /= bName = a --если имена таблиц разные то возвращаем первую таблицу
        | aName == bName = Table aName $ getTableBody a ++ getTableBody b --если одинаковые - то новую с суммой полей
        where
            aName = tName a
            bName = tName b
    

type Scheme = S.Set Table --типа база

--при конвертации поля друг другу соответствуют. 
--для отличия 
type CorrespondFrom = (TableName, FieldName)
type CorrespondTo  = (TableName, FieldName)

data Corresponds = Corresponds [(CorrespondTo, CorrespondFrom)]




