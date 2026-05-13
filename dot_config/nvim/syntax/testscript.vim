" Vim syntax file
" Language:     build2 testscript
" Maintainer:   William Roy <wroy@proton.me>
" Filenames:    testscript, *.testscript
" URL:          https://build2.org
"
" References:
"   - build2 Testscript Manual: https://build2.org/build2/doc/build2-testscript-manual.xhtml

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

" =============================================================================
" Keywords and Control Flow
" =============================================================================

" Directives
syntax match testscriptDirective /^\s*\.\s*include\>/
syntax match testscriptDirective /^\s*\.\s*include\s\+--once\>/

" Conditional statements
syntax keyword testscriptConditional if if! ifn ifn! ife ife!
syntax keyword testscriptConditional elif elif! elifn elifn! elife elife!
syntax keyword testscriptConditional else

" Loop constructs
syntax keyword testscriptRepeat for while continue break

" Built-in operations
syntax keyword testscriptBuiltin cat cp date diff echo env exit export false find ln mkdir mv rm rmdir sed set sha256sum sleep test timeout touch true xxh64sum

" =============================================================================
" Descriptions
" =============================================================================

" Leading and trailing descriptions
syntax match testscriptDescription /^\s*:\s.*$/
syntax match testscriptDescription /\s\+:\s.*$/

" =============================================================================
" Setup and Teardown
" =============================================================================

" Setup and teardown prefixes
syntax match testscriptSetup /^\s*+/
syntax match testscriptTeardown /^\s*-/

" =============================================================================
" Properties and Attributes
" =============================================================================

" Type annotations in square brackets
syntax region testscriptTypeAnnotation start=/\[/ end=/\]/

" Type keywords
syntax match testscriptTypeKeyword /\[\s*\zs\<bool\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<int64\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<int64s\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<uint64\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<uint64s\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<string\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<strings\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<string_set\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<string_map\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<path\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<paths\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<dir_path\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<dir_paths\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<json\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<json_array\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<json_object\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<json_set\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<json_map\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<name\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<names\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<name_pair\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<project_name\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<target_triplet\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<cmdline\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation
syntax match testscriptTypeKeyword /\[\s*\zs\<null\>\ze\s*\]/ contained containedin=testscriptTypeAnnotation

" =============================================================================
" Variables and Interpolation
" =============================================================================

" Variable expansion: $var, $(var), $(<), $(>), $*
syntax match testscriptVariable /\$[[:alpha:]][[:alnum:]_.]*/
syntax match testscriptVariable /\$([[:alpha:]][[:alnum:]_.]*)/
syntax match testscriptVariable /\$([<>])/
syntax match testscriptVariable /\$(\*)/
syntax match testscriptVariable /\$\*/

" =============================================================================
" Operators and Punctuation
" =============================================================================

" Assignment operators
syntax match testscriptOperator /=/
syntax match testscriptOperator /+=/
syntax match testscriptOperator /=+/

" Logical operators
syntax match testscriptOperator /&&/
syntax match testscriptOperator /||/
syntax match testscriptOperator /|/

" Comparison operators
syntax match testscriptOperator /==/
syntax match testscriptOperator /!=/

" Redirect and cleanup operators
syntax match testscriptOperator /<-/
syntax match testscriptOperator /<|/
syntax match testscriptOperator /<=/
syntax match testscriptOperator /<<</
syntax match testscriptOperator /<<=/
syntax match testscriptOperator /<</
syntax match testscriptOperator /</
syntax match testscriptOperator />-/
syntax match testscriptOperator />|/
syntax match testscriptOperator />!/
syntax match testscriptOperator />=/
syntax match testscriptOperator />+/
syntax match testscriptOperator />&/
syntax match testscriptOperator />>>/
syntax match testscriptOperator />?/
syntax match testscriptOperator />>?/
syntax match testscriptOperator />>>?/
syntax match testscriptOperator />>/
syntax match testscriptOperator />/
syntax match testscriptOperator /&\?/
syntax match testscriptOperator /&!/
syntax match testscriptOperator /&/

" Block delimiters
syntax match testscriptDelimiter /[{}();]/

" =============================================================================
" Literals
" =============================================================================

" Escape sequences in strings
syntax match testscriptEscape contained /\\[nrt\\'"]/
syntax match testscriptEscape contained /\\\$/
syntax match testscriptEscape contained /\\[0-7]\{1,3}/
syntax match testscriptEscape contained /\\x[0-9a-fA-F]\{1,2}/
syntax match testscriptEscape contained /\\u[0-9a-fA-F]\{4}/
syntax match testscriptEscape contained /\\U[0-9a-fA-F]\{8}/

" Double quoted strings
syntax region testscriptString start=/"/ skip=/\\./ end=/"/ contains=testscriptEscape,testscriptVariable

" Single quoted (raw strings)
syntax region testscriptRawString start=/'/ end=/'/

" Multi-line strings
syntax region testscriptMultiString start=/"""/ end=/"""/ contains=testscriptEscape,testscriptVariable

" Regex literals (e.g. ~/regex/ or >~/regex/ in redirects)
syntax region testscriptRegex start=/\~"/ skip=/\\./ end=/"/ contains=testscriptEscape
syntax region testscriptRegex start=/\~\// skip=/\\./ end=/\// contains=testscriptEscape
syntax region testscriptRegex start=/\~%/ skip=/\\./ end=/%/ contains=testscriptEscape

" Numbers
syntax match testscriptNumber /\%([[:alnum:]_]++\)\@<!\<\d\+\>/
syntax match testscriptNumber /\<0x\x\+\>/
syntax match testscriptNumber /\<\d\+\.\d\+\>/

" Boolean
syntax keyword testscriptBoolean true false

" Null
syntax keyword testscriptNull null

" =============================================================================
" Comments
" =============================================================================

" Single-line comments
syntax region testscriptComment start=/#/ end=/$/ contains=testscriptTodo,@Spell

" Block comments
syntax region testscriptBlockComment start=/^\s*#\\$/ end=/^\s*#\\$/ contains=testscriptTodo,@Spell

" TODO/FIXME/XXX/NOTE markers in comments
syntax keyword testscriptTodo contained TODO FIXME XXX NOTE HACK COMBAK
syntax match testscriptTodo /@@:/ contained

" =============================================================================
" Special Constructs
" =============================================================================

" Line continuation
syntax match testscriptLineContinuation /\\\n\s*/ contained

" Function calls
syntax match testscriptFunction /\$[[:alpha:]][[:alnum:]_.]*\s*(/me=e-1

" =============================================================================
" Highlight Linking
" =============================================================================

highlight default link testscriptComment           Comment
highlight default link testscriptBlockComment      Comment
highlight default link testscriptTodo              Todo

highlight default link testscriptDirective         PreProc
highlight default link testscriptConditional       Conditional
highlight default link testscriptRepeat            Repeat
highlight default link testscriptBuiltin           Function
highlight default link testscriptFunction          Function

highlight default link testscriptDescription       SpecialComment
highlight default link testscriptSetup             Special
highlight default link testscriptTeardown          Special

highlight default link testscriptTypeAnnotation    Type
highlight default link testscriptTypeKeyword       Type

highlight default link testscriptString            String
highlight default link testscriptRawString         String
highlight default link testscriptMultiString       String
highlight default link testscriptRegex             String

highlight default link testscriptNumber            Number
highlight default link testscriptBoolean           Boolean
highlight default link testscriptNull              Constant

highlight default link testscriptVariable          Identifier

highlight default link testscriptOperator          Operator
highlight default link testscriptDelimiter         Delimiter

highlight default link testscriptLineContinuation  Special
highlight default link testscriptEscape            SpecialChar

" Restore compatibility options
let &cpo = s:cpo_save
unlet s:cpo_save

let b:current_syntax = "testscript"

" vim: ts=2 sw=2 et
