" Meowscript syntax file.
" Language: Meowscript
" Maintainer: Kelly Bethany <kelly.a.betty@gmail.com>

if exists("b:current_syntax")
    finish
endif

syntax case match

syntax keyword meowsLonely lonely
syntax keyword meowsBool happy sad

" Keywords
syntax keyword meowsKeyword meow mew meowmeow take while run off bring takes as do catnap purr watch catch hiss

" Constants
syntax match meowsInt /\d\+/
syntax match meowsFloat /\d\+\.\d\+/
syntax region meowsString start=/"/ skip=/\\"/ end=/"/

" Operators
syntax keyword meowsOperator and or not knock over push peek
syntax match meowsOperator /+/
syntax match meowsOperator /-/
syntax match meowsOperator /\*/
syntax match meowsOperator /\//
syntax match meowsOperator /\^/
syntax match meowsOperator /\./
syntax match meowsOperator /\~\~/
syntax match meowsOperator /[=!]\==/
syntax match meowsOperator /[<>]=\=/
syntax match meowsOperator /\[/
syntax match meowsOperator /\]/
syntax match meowsOperator /(/
syntax match meowsOperator /)/
syntax match meowsOperator /?/
syntax match meowsOperator /:/
syntax match meowsOperator /%/
syntax match meowsOperator /!/
syntax match meowsOperator /\\\n/

" Expressions
syntax match meowsFunc /=\^\.[xX]\.\^=/
syntax match meowsBox /\~( \^\.[xX]\.\^) BOX!!/
syntax match meowsLambda /[^~]( \^\.[xX]\.\^)>/

syntax match meowsLineComment /--.*$/
syntax region meowsBlockComment start=/\~( \^\.[xX]\.\^)>/ end=/<(\^\.[xX]\.\^ )\~/

" Identifiers
syntax match meowsIdentifier /[a-zA-Z'_][a-zA-Z0-9'_]*/


" Adding highlighting:
" --------------------------------
let b:current_syntax = "meowscript"

" Constants:
hi def link meowsString String
hi def link meowsInt Number
hi def link meowsFloat Float
hi def link meowsBool Boolean
hi def link meowsLonely Keyword

" Keywords:
hi def link meowsKeyword Keyword

" Operators:
hi def link meowsOperator Operator

" Expressions:
hi def link meowsFunc Statement
hi def link meowsBox Type
hi def link meowsLambda Type

" Comments:
hi def link meowsLineComment Comment
hi def link meowsBlockComment Comment

" Identifiers:
hi def link meowsIdentifier Identifier
