module Database.SQL.SQLConverter.Types (
    FieldType,
    FieldName,
    TableName,
	RelationField,
	Field (Regular, Key, Relation),
	Table (Table),
    Scheme,
    TableOperations (tName,tBody),
    RelationInGraph (RelationInGraph)
	
) where
import qualified Data.Text as T 
import qualified Data.Set as S


type FieldType = T.Text --RegularField|Key|Relation
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

--сделано чтобы присвоить вес ребрам. вес ребра - еденица.
data RelationInGraph = RelationInGraph ((TableName, FieldName),(TableName, FieldName)) deriving (Eq, Ord, Show)

instance Num RelationInGraph where
    (+) a b = 2
    (*) a b = 1
    abs a = 1
    signum a = 1
    fromInteger a = RelationInGraph ((T.pack "",T.pack ""),(T.pack "",T.pack ""))
    negate a = 1
instance Real RelationInGraph where
    toRational a =  1
    



