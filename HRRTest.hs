import Data.Int (Int32)
import Database.Relational.Query

hello :: Relation () (Int32, String)
hello = relation $ return (value 0 >< value "Hello")

world :: Relation () (Int32, String)
world = relation $ return (value 0 >< value "World")

everywhere :: Relation () (Int32, String)
everywhere = relation $ return (value 0 >< value "Everywhere!")


helloWorld :: Relation () (Int32, (String, String, Int))
helloWorld = relation $ do
    h <- query hello
    w <- query world
	e <- query everywhere
    on $ h ! fst' .=. w ! fst' .=. e ! fst'
    return $ h ! fst' >< ((h ! snd' >< w ! snd') >< ( w ! fst' >< e ! fst'))

main :: IO ()
main = putStrLn $ show helloWorld 
--SELECT ALL T0.f0 AS f0, T0.f1 AS f1, T1.f1 AS f2 FROM (SELECT ALL 0 AS f0, 'Hello' AS f1) T0 INNER JOIN (SELECT ALL 0 AS f0, 'World!' AS f1) T1 ON (T0.f0 = T1.f0);








