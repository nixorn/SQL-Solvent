module Database.SQL.SQLConverter.Types (
    FieldType,
    FieldName,
	RelationField,
	Field (RegularField,Key,Relation),
	Table (Table),
    Corresponds(Corresponds),
    Scheme
    
	
) where
import qualified Data.Text as T (Text)
--
type FieldType = T.Text --используется только в сыром представлении. RegularField|Key|Relation
type DataType = T.Text --тип данных поля
type FieldName = T.Text 
type TableName = T.Text 
type RelationField = T.Text --куда поле указывает


data Field = RegularField FieldName DataType 
	| Key FieldName DataType 
	| Relation FieldName DataType TableName RelationField 

data Table = Table TableName [Field] 

class TableOperations Table where
    (+++) :: Table -> Table -> Table --суммировать поля
    getTableName :: Table -> TableName
    getTableBody :: Table -> [Field]

instance TableOperations Table where
    getTableName (Table name _) = name
    getTableBody (Table _ body) = body
    (+++) a  b  
        | aName /= bName = a --если имена таблиц разные то возвращаем первую таблицу
        | aName == bName = Table aName $ getTableBody a ++ getTableBody b --если одинаковые - то новую с суммой полей
        where
            aName = getTableName a
            bName = getTableName b
    

type Scheme = [Table]--типа база

--при конвертации поля друг другу соответствуют. 
--для отличия 
type CorrespondFrom = (TableName, FieldName)
type CorrespondTo  = (TableName, FieldName)

data Corresponds = Corresponds [(CorrespondTo, CorrespondFrom)]




