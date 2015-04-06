import Database.SQL.SQLSolver.Types


from1 = Table [ Key "OUID",
				RegularField "SURNAME",
				RegularField "NAME",
				RegularField "SECONDNAME"]

to1 = Table [ Key "ID",
				RegularField "FAM",
				RegularField "IM",
				RegularField "OTCH"]