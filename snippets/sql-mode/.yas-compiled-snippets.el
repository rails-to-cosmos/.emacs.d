;;; Compiled snippets and support files for `sql-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sql-mode
                     '(("references" "REFERENCES ${1:TableName}([${2:ColumnName}])\n" "REFERENCES ..." nil nil nil "/Users/akatovda/.emacs.d/snippets/sql-mode/references" nil nil)
                       ("ins" "INSERT INTO ${table-name} (${col1, col2}) VALUES (${'val1', 'val2'});$0" "insert" nil nil nil "/Users/akatovda/.emacs.d/snippets/sql-mode/insert" nil nil)
                       ("create.1" "CREATE PROCEDURE [${1:dbo}].[${2:Name}] \n(\n		$3		$4		= ${5:NULL}		${6:OUTPUT}\n)\nAS\nBEGIN\n$0\nEND\nGO\n" "create procedure ..." nil nil nil "/Users/akatovda/.emacs.d/snippets/sql-mode/create.1" nil nil)
                       ("create" "CREATE TABLE [${1:dbo}].[${2:TableName}] \n(\n		${3:Id}		${4:INT IDENTITY(1,1)}		${5:NOT NULL}\n$0\n	CONSTRAINT [${6:PK_}] PRIMARY KEY ${7:CLUSTERED} ([$3]) \n)\nGO\n" "create table ..." nil nil nil "/Users/akatovda/.emacs.d/snippets/sql-mode/create" nil nil)
                       ("constraint.1" "CONSTRAINT [${1:FK_Name}] FOREIGN KEY ${2:CLUSTERED} ([${3:ColumnName}]) \n" "CONSTRAINT [..] FOREIGN KEY ..." nil nil nil "/Users/akatovda/.emacs.d/snippets/sql-mode/constraint.1" nil nil)
                       ("constraint" "CONSTRAINT [${1:PK_Name}] PRIMARY KEY ${2:CLUSTERED} ([${3:ColumnName}]) \n" "CONSTRAINT [..] PRIMARY KEY ..." nil nil nil "/Users/akatovda/.emacs.d/snippets/sql-mode/constraint" nil nil)
                       ("column" "	,	${1:Name}		${2:Type}			${3:NOT NULL}\n" ", ColumnName ColumnType NOT NULL..." nil nil nil "/Users/akatovda/.emacs.d/snippets/sql-mode/column" nil nil)))


;;; Do not edit! File generated at Tue Oct 18 15:50:03 2016
