module Database.SQL.SQLConverter.Types (
    FieldType,
    FieldName,
	RelationField,
	Field (RegularField,Key,Relation),
	Table (Table),
    Corresponds(Corresponds),
    Scheme,
    RawSchemeEntry,
    RawScheme
    
	
) where


type FieldType = String --используется только в сыром представлении. RegularField|Key|Relation
type DataType = String --тип данных поля
type FieldName = String 
type TableName = String 
type RelationField = String --куда поле указывает


data Field = RegularField FieldName DataType 
	| Key FieldName DataType 
	| Relation FieldName DataType TableName RelationField 

data Table = Table TableName [Field] 

type Scheme = [Table]--типа база
type RawSchemeEntry = (FieldType, DataType, TableName, FieldName, TableName, FieldName)
type RawScheme = [RawSchemeEntry] --схема считана откуда то сырой таблицей

--при конвертации поля друг другу соответствуют. 
--для отличия 
type CorrespondFrom = (TableName, FieldName)
type CorrespondTo  = (TableName, FieldName)

data Corresponds = Corresponds [(CorrespondTo, CorrespondFrom)]


