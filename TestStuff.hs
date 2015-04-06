module TestStuff (
    to,
    from
) where


import Database.SQL.SQLConverter.Types

to = [
    --личное дело
    Table "PERSONAL_PROFILE" [ 
        Key "UID" "int",
        RegularField "UNICAL_CODE" "varchar",
        Relation "SURNAME" "int" "SURNAMES" "UID",
        Relation "NAME" "int" "NAMES" "UID",
        Relation "SECONDRNAME" "int" "SECONDNAMES" "UID",
        RegularField "BIRTHDATE" "datetime"],
        
    Table "SURNAMES" [
        Key "UID" "int",
        RegularField "NAME" "varchar"
        ],
        
    Table "NAMES" [
        Key "UID" "int",
        RegularField "NAME" "varchar"
        ],
        
    Table "SECONDNAMES"[
        Key "UID" "int",
        RegularField "NAME" "varchar"
        ],
        
    --заявление на услугу
    Table "PETITION" [
        Key "UID" "int",
        --подчинено личному делу. ссылка на получателя
        Relation "PROFILE" "int" "PERSONAL_PROFILE" "UID",
        --ссылка на представителя. может быть пустой.
        Relation "PROFILE_REPRESENT" "int" "PERSONAL_PROFILE" "UID",
        RegularField "DATE" "datetime",
        RegularField "NUMBER" "varchar",
        --подтверждено или отклонено
        RegularField "APPROOVED" "int"],
        
    --заявка на услугу
    Table "APPLICATION" [
        Key "UID" "int",
        --подчинена заявлению
        Relation "PETITION" "int" "PETITION" "UID",
        RegularField "DATE" "datetime",
        RegularField "NUMBER" "varchar"],
        
    --вспомогательная таблица для связи многие-ко-многим между заявлениями и приколотыми к заявлениям документами
    Table "PETITION_DOCUMENTS_TO" [
        Key "UID" "int",
        Relation "PETITION" "int" "PETITION" "UID",
        Relation "PERSONAL_DOCUMENTS" "int" "PERSONAL_DOCUMENTS" "UID"],
        
    --направление на получение услуги. получение этого документа - цель заявителя.
    Table "DIRECTION" [
        Key "UID" "int",
        RegularField "NUMBER" "varchar",
        RegularField "DATE" "varchar",
        Relation "ORGANIZATION" "int" "ORGANIZATION" "UID"],
        
    --документы человека
    Table "PERSONAL_DOCUMENTS" [ 
        Key "UID" "int",
        Relation "PROFILE" "int" "PERSONAL_PROFILE" "UID",
        --он же тип
        RegularField "NAME" "varchar",
        RegularField "SERIES" "varchar",
        RegularField "NUMBER" "varchar",
        RegularField "ISSUEDATE" "datetime",
        Relation "ISSUEORG" "int" "ORGANIZATIONS" "UID"],
		
    --организации
    Table "ORGANIZATIONS" [
        Key "UID" "int",
        RegularField "NAME" "varchar" ,
        RegularField "ADRESS" "varchar",
        RegularField "INN" "varchar"]
        ]


from = [
    --личное дело
    Table "LD" [ 
        Key "ID" "int",
        RegularField "FAM" "varchar",
        RegularField "IM" "varchar",
        RegularField "OTCH" "varchar",
        RegularField "DROZHD" "DATETIME",
        RegularField "KOD_CHELOVEKA" "varchar"],
        
    --заявка
    Table "QUEUE" [
        Key "ID" "int",
        Relation "DELO" "int" "LD" "ID",
        --дата заявления
        RegularField "DATA_ZAYAV" "datetime",
        RegularField "IM_PREDST" "varchar",
        RegularField "FAM_PREDST" "varchar",
        RegularField "OTCH_PREDST" "varchar",
        RegularField "KOD_PREDST" "varchar",
        RegularField "DROZHD_PREDST" "varchar"],
        
    --поставщики
    Table "POSTAV" [
        Key "ID" "int",
        RegularField "NAZV" "varchar",
        RegularField "INN" "varchar"],
        
    --организации, выдающие документы людям
    Table "DEPARTAMENT" [
        Key "ID" "int",
        RegularField "NAZV" "varchar"]
    ]
    
corresponds =  Corresponds [
    --люди
    (("PERSONAL_PROFILE", "UID")        ,("LD", "ID")),
    (("PERSONAL_PROFILE", "UNICAL_CODE"),("LD", "KOD_CHELOVEKA")),
    (("PERSONAL_PROFILE", "BIRTHDATE")  ,("LD", "DROZHD")),
    (("SECONDNAMES"     , "NAME")       ,("LD", "FAM")),
    (("SURNAMES"        , "NAME")       ,("LD", "IM")),
    (("NAMES"           , "NAME")       ,("LD", "OTCH"))
    
    ]
    
    
    

				
				