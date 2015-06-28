# SQL-Solvent

<img src="https://github.com/nixorn/SQL-Solvent/blob/master/logo.png?raw=true" align="left" hspace="10" vspace="6">

**SQL-Solvent** Программа для исследования баз данных.





# Как использовать
Нужно две вещи - структура базы и браузер.

# Стуктура
Сейчас поддерживается только импорт из CSV. CSV-файл должен иметь 8 полей:
1. Тип поля должен иметь одно из трех значений : "Regular"|"Key"|"Relation". Это соответствует полям, содержащим данные | Ключам | Реляционным ссылкам на другие таблицы соответственно;
2. Тип данных в поле;
3. Название таблицы;
4. Название поля;
5. Опциональное для ссылок значение. Имя таблицы, на которую ссылается данное поле;
6. Опциональное для ссылок значение. Имя поля, на которое ссылается данное поле;
7. Описание таблицы;
8. Описание поля.

Пример строчки файла
```csv
"Regular";"varchar";"A_ASP_UCH";"A_BUILDING";"NULL";"NULL";"Правило для зоны доставочного участка";"Номер корпуса";;
```

# Самостоятельная сборка

Во-первых Firstly, you need the Haskell Platform (https://www.haskell.org/platform/). Install it. Then say in shell:
 
```
cabal update
```
If you on Win, type this also:
```
cabal install cabal-install
```
That will upgrade your platform.
If you on *nix - do upgrading as you know.

Secondly, clone repository, cd into and say:
```
cabal sandbox init
cabal install
cabal build
```

run ./dist/build/solvent(.exe), go 127.0.0.1:8000

static files in ./static . Copy ./static in directory, where you run solvent(.exe)