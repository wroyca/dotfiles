" Vim syntax file
" Language:     build2 buildfile
" Maintainer:   William Roy <wroy@proton.me>
" Filenames:    buildfile, *.build
" URL:          https://build2.org
"
" References:
"   - build2 Build System Manual: https://build2.org/build2/doc/build2-build-system-manual.xhtml

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

" =============================================================================
" Keywords and Control Flow
" =============================================================================

" Directives
syntax match buildfileDirective /\%([-+:]\)\@<!\<import\>\%([-+:]\)\@!/
syntax match buildfileDirective /\%([-+:]\)\@<!\<include\>\%([-+:]\)\@!/
syntax match buildfileDirective /\%([-+:]\)\@<!\<source\>\%([-+:]\)\@!/
syntax match buildfileDirective /\%([-+:]\)\@<!\<define\>\%([-+:]\)\@!/
syntax match buildfileDirective /\%([-+:.]\)\@<!\<export\>\%([-+:.]\)\@!/
syntax keyword buildfileDirective run print info warn fail dump assert
syntax match buildfileDirective /\%(using\%(?\)\?\s\+\)\@<!\<config\>/ nextgroup=buildfileVariable skipwhite

" Conditional statements
syntax keyword buildfileConditional if elif else switch case default
syntax keyword buildfileConditional nextgroup=buildfileCondition skipwhite

" Loop constructs
syntax keyword buildfileRepeat for

" Scope and module keywords
syntax keyword buildfileKeyword project extension backlink

" Built-in operations
syntax keyword buildfileKeyword install dist clean

" Project metadata keywords
syntax match buildfileKeyword /^\s*\<version\>\s*=/me=e-1
syntax match buildfileKeyword /^\s*\<summary\>\s*=/me=e-1
syntax match buildfileKeyword /^\s*\<description\>\s*=/me=e-1
syntax match buildfileKeyword /^\s*\<license\>\s*=/me=e-1
syntax match buildfileKeyword /^\s*\<url\>\s*=/me=e-1
syntax match buildfileKeyword /^\s*\<email\>\s*=/me=e-1

" Language/module keywords
syntax match buildfileKeyword /\%([-+:.]\)\@<!\<namespace\>\%([-+:.]\)\@!/

" Using directive
syntax match buildfileUsingDirective /\<using\%(?\)\?/

" Module names
syntax match buildfileModuleName /\%(using\%(?\)\?\s\+\)\@<=\<\%(version\|config\|dist\|test\|install\|cxx\|cc\|c\)\>/

" =============================================================================
" Built-in Functions and Script Commands
" =============================================================================

" File system operations
syntax keyword buildfileBuiltin cat cp ln mkdir mv rm rmdir touch

" Text processing
syntax keyword buildfileBuiltin diff echo sed

" Control flow and testing
syntax keyword buildfileBuiltin exit set sleep

" Test
syntax match buildfileBuiltin /\%(using\%(?\)\?\s\+\)\@<!\<test\>/

" Environment and system
syntax keyword buildfileBuiltin date env

" Output and diagnostics
syntax keyword buildfileBuiltin info text warn fail print assert dump

" =============================================================================
" Properties and Attributes (defined before target types for priority)
" =============================================================================

" Common variable namespaces and properties
syntax match buildfileProperty /\<config\.[[:alnum:]_.]\+/
syntax match buildfileProperty /\<install\.[[:alnum:]_.]\+/
syntax match buildfileProperty /\<dist\.[[:alnum:]_.]\+/
syntax match buildfileProperty /\<clean\.[[:alnum:]_]\+/
syntax match buildfileProperty /\<backlink\.[[:alnum:]_.]\+/

" Compiler and linker options
syntax match buildfileProperty /\<c\.poptions\>/
syntax match buildfileProperty /\<c\.coptions\>/
syntax match buildfileProperty /\<c\.loptions\>/
syntax match buildfileProperty /\<c\.aoptions\>/
syntax match buildfileProperty /\<c\.libs\>/
syntax match buildfileProperty /\<c\.export\.[[:alnum:]_.]\+/

syntax match buildfileProperty /\<cc\.poptions\>/
syntax match buildfileProperty /\<cc\.coptions\>/
syntax match buildfileProperty /\<cc\.loptions\>/
syntax match buildfileProperty /\<cc\.aoptions\>/
syntax match buildfileProperty /\<cc\.libs\>/
syntax match buildfileProperty /\<cc\.export\.[[:alnum:]_.]\+/

syntax match buildfileProperty /\<cxx\.poptions\>/
syntax match buildfileProperty /\<cxx\.coptions\>/
syntax match buildfileProperty /\<cxx\.loptions\>/
syntax match buildfileProperty /\<cxx\.aoptions\>/
syntax match buildfileProperty /\<cxx\.libs\>/
syntax match buildfileProperty /\<cxx\.std\>/
syntax match buildfileProperty /\<cxx\.export\.[[:alnum:]_.]\+/

" Library versioning
syntax match buildfileProperty /\<bin\.lib\.version\>/
syntax match buildfileProperty /\<lib\.version\>/

" Installation properties
syntax match buildfileProperty /\<install\.subdirs\>/
syntax match buildfileProperty /\<install\.include\>/
syntax match buildfileProperty /\<install\.lib\>/
syntax match buildfileProperty /\<install\.bin\>/
syntax match buildfileProperty /\<install\.sbin\>/
syntax match buildfileProperty /\<install\.data\>/
syntax match buildfileProperty /\<install\.doc\>/
syntax match buildfileProperty /\<install\.man\>/
syntax match buildfileProperty /\<install\.share\>/

" =============================================================================
" Target Types and Type Annotations
" =============================================================================

" Non standard target types (e.g., cli, cli.cxx, foo.c, custom.type)
syntax match buildfileSpecialTargetType /[[:alnum:]_]\+\.[[:alnum:]_]\+\ze{/
syntax match buildfileSpecialTargetType /[[:alnum:]_]\+\ze{/
syntax match buildfileSpecialTargetType /{\zs[[:alnum:]_]\+\.[[:alnum:]_]\+\ze}/
syntax match buildfileSpecialTargetType /{\zs[[:alnum:]_]\+\ze}/

" Standard built-in target types
syntax match buildfileTargetType /\<exe\>\ze{/
syntax match buildfileTargetType /\<lib\>\ze{/
syntax match buildfileTargetType /\<libs\>\ze{/
syntax match buildfileTargetType /\<liba\>\ze{/
syntax match buildfileTargetType /\<libso\>\ze{/
syntax match buildfileTargetType /\<libue\>\ze{/
syntax match buildfileTargetType /\<obj\>\ze{/
syntax match buildfileTargetType /\<obja\>\ze{/
syntax match buildfileTargetType /\<objs\>\ze{/
syntax match buildfileTargetType /\<obje\>\ze{/
syntax match buildfileTargetType /\<h\>\ze{/
syntax match buildfileTargetType /\<hxx\>\ze{/
syntax match buildfileTargetType /\<ixx\>\ze{/
syntax match buildfileTargetType /\<txx\>\ze{/
syntax match buildfileTargetType /\<mxx\>\ze{/
syntax match buildfileTargetType /\<cxx\>\ze{/
syntax match buildfileTargetType /\<c\>\ze{/
syntax match buildfileTargetType /\<cc\>\ze{/
syntax match buildfileTargetType /\<C\>\ze{/
syntax match buildfileTargetType /\<cpp\>\ze{/
syntax match buildfileTargetType /\<c++\>\ze{/
syntax match buildfileTargetType /\<cp\>\ze{/
syntax match buildfileTargetType /\<doc\>\ze{/
syntax match buildfileTargetType /\<man\>\ze{/
syntax match buildfileTargetType /\<legal\>\ze{/
syntax match buildfileTargetType /\<file\>\ze{/

" Target types inside braces: {type}
syntax match buildfileTargetType /{\zs\<exe\>\ze}/
syntax match buildfileTargetType /{\zs\<lib\>\ze}/
syntax match buildfileTargetType /{\zs\<libs\>\ze}/
syntax match buildfileTargetType /{\zs\<liba\>\ze}/
syntax match buildfileTargetType /{\zs\<libso\>\ze}/
syntax match buildfileTargetType /{\zs\<libue\>\ze}/
syntax match buildfileTargetType /{\zs\<obj\>\ze}/
syntax match buildfileTargetType /{\zs\<obja\>\ze}/
syntax match buildfileTargetType /{\zs\<objs\>\ze}/
syntax match buildfileTargetType /{\zs\<obje\>\ze}/
syntax match buildfileTargetType /{\zs\<h\>\ze}/
syntax match buildfileTargetType /{\zs\<hxx\>\ze}/
syntax match buildfileTargetType /{\zs\<ixx\>\ze}/
syntax match buildfileTargetType /{\zs\<txx\>\ze}/
syntax match buildfileTargetType /{\zs\<mxx\>\ze}/
syntax match buildfileTargetType /{\zs\<cxx\>\ze}/
syntax match buildfileTargetType /{\zs\<c\>\ze}/
syntax match buildfileTargetType /{\zs\<cc\>\ze}/
syntax match buildfileTargetType /{\zs\<C\>\ze}/
syntax match buildfileTargetType /{\zs\<cpp\>\ze}/
syntax match buildfileTargetType /{\zs\<c++\>\ze}/
syntax match buildfileTargetType /{\zs\<cp\>\ze}/
syntax match buildfileTargetType /{\zs\<doc\>\ze}/
syntax match buildfileTargetType /{\zs\<man\>\ze}/
syntax match buildfileTargetType /{\zs\<legal\>\ze}/
syntax match buildfileTargetType /{\zs\<file\>\ze}/

" Type annotations in square brackets
syntax region buildfileTypeAnnotation start=/\[/ end=/\]/

" Type keywords
syntax match buildfileTypeKeyword /\[\s*\zs\<bool\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<uint64\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<uint64s\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<string\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<strings\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<path\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<paths\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<dir_path\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<dir_paths\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<name\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<names\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<name_pair\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<project_name\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<target_triplet\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<cmdline\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation
syntax match buildfileTypeKeyword /\[\s*\zs\<null\>\ze\s*\]/ contained containedin=buildfileTypeAnnotation

" =============================================================================
" Variables and Interpolation
" =============================================================================

" Variable expansion: $var, $(var), $(<), $(>)
syntax match buildfileVariable /\$[[:alpha:]][[:alnum:]_.]*/
syntax match buildfileVariable /\$([[:alpha:]][[:alnum:]_.]*)/
syntax match buildfileVariable /\$([<>])/
syntax match buildfileVariable /\$(\*)/

" Special variables and attributes
syntax match buildfileSpecialVar /\<src_root\>/
syntax match buildfileSpecialVar /\<out_root\>/
syntax match buildfileSpecialVar /\<src_base\>/
syntax match buildfileSpecialVar /\<out_base\>/
syntax match buildfileSpecialVar /\<project\>/
syntax match buildfileSpecialVar /\<amalgamation\>/
syntax match buildfileSpecialVar /\<subprojects\>/

" =============================================================================
" Operators and Punctuation
" =============================================================================

" Assignment operators
syntax match buildfileOperator /=/
syntax match buildfileOperator /+=/
syntax match buildfileOperator /=+/

" Dependency operators
syntax match buildfileOperator /:/
syntax match buildfileOperator /<:/

" Pattern and path operators
syntax match buildfileOperator /\~/
syntax match buildfileOperator /\*\*/
syntax match buildfileOperator /\*/
syntax match buildfileOperator /\./

" Exclusion operator
syntax match buildfileOperator /-/

" Logical operators
syntax match buildfileOperator /&&/
syntax match buildfileOperator /||/
syntax match buildfileOperator /!/

" Modifier operators (import!, import?, using?, define?, etc.)
syntax match buildfileOperator /?/

" Ad-hoc target override operator
syntax match buildfileOperator /@/

" Comparison operators
syntax match buildfileOperator /==/
syntax match buildfileOperator /!=/
syntax match buildfileOperator /<=/
syntax match buildfileOperator />=/
syntax match buildfileOperator /</
syntax match buildfileOperator />/

" Block delimiters
syntax match buildfileDelimiter /[{}()]/

" =============================================================================
" Literals
" =============================================================================

" Escape sequences in strings
syntax match buildfileEscape contained /\\[nrt\\'"]/
syntax match buildfileEscape contained /\\\$/
syntax match buildfileEscape contained /\\[0-7]\{1,3}/
syntax match buildfileEscape contained /\\x[0-9a-fA-F]\{1,2}/
syntax match buildfileEscape contained /\\u[0-9a-fA-F]\{4}/
syntax match buildfileEscape contained /\\U[0-9a-fA-F]\{8}/

" Double quoted strings
syntax region buildfileString start=/"/ skip=/\\./ end=/"/ contains=buildfileEscape,buildfileVariable

" Single quoted (raw strings)
syntax region buildfileRawString start=/'/ end=/'/

" Multi-line strings
syntax region buildfileMultiString start=/"""/ end=/"""/ contains=buildfileEscape,buildfileVariable

" Regex literals
syntax region buildfileRegex start=/\~"/ skip=/\\./ end=/"/ contains=buildfileEscape

" Numbers
syntax match buildfileNumber /\%([[:alnum:]_]++\)\@<!\<\d\+\>/
syntax match buildfileNumber /\<0x\x\+\>/
syntax match buildfileNumber /\<\d\+\.\d\+\>/

" Boolean
syntax keyword buildfileBoolean true false

" Null
syntax keyword buildfileNull null

" =============================================================================
" Paths
" =============================================================================

" Directory paths with trailing slash
syntax match buildfilePath /\.\.\?\/\|\*\/\|[[:alnum:]_][[:alnum:]_./\-]*\//

" =============================================================================
" Comments
" =============================================================================

" Single-line comments
syntax region buildfileComment start=/#/ end=/$/ contains=buildfileTodo,@Spell

" Block comments
syntax region buildfileBlockComment start=/^\s*#\\$/ end=/^\s*#\\$/ contains=buildfileTodo,@Spell

" TODO/FIXME/XXX/NOTE markers in comments
"
" Note that only TODO is formally defined by the default highlight set; the
" remaining markers are mapped to it for consistency. Dedicated pattern plugins
" (for example, mini.hipatterns) can provide finer granularity if distinct
" groups are desirable.
"
syntax keyword buildfileTodo contained TODO FIXME XXX NOTE HACK COMBAK
syntax match buildfileTodo /@@:/ contained

" =============================================================================
" Special Constructs
" =============================================================================

" Line continuation
syntax match buildfileLineContinuation /\\\n\s*/ contained

" Attribute specification
syntax match buildfileAttribute /@[[:alnum:]_]\+/

" Function calls
syntax match buildfileFunction /\$[[:alpha:]][[:alnum:]_.]*\s*(/me=e-1

" =============================================================================
" Regions and Blocks
" =============================================================================

" Recipe blocks (commands to execute)
syntax region buildfileRecipe start=/{{/ end=/}}/ contains=buildfileBuiltin,buildfileString,buildfileVariable,buildfileComment,buildfileLineContinuation

" Conditional blocks
syntax region buildfileCondBlock start=/{/ end=/}/ transparent contains=ALL

" =============================================================================
" Highlight Linking
" =============================================================================

highlight default link buildfileComment           Comment
highlight default link buildfileBlockComment      Comment
highlight default link buildfileTodo              Todo

highlight default link buildfileDirective         PreProc
highlight default link buildfileUsingDirective    PreProc
highlight default link buildfileConditional       Conditional
highlight default link buildfileRepeat            Repeat
highlight default link buildfileKeyword           Keyword
highlight default link buildfileModuleName        Keyword

highlight default link buildfileBuiltin           Function
highlight default link buildfileFunction          Function

highlight default link buildfileTargetType        Type
highlight default link buildfileSpecialTargetType Special
highlight default link buildfileTypeAnnotation    Type
highlight default link buildfileTypeKeyword       Type

highlight default link buildfileString            String
highlight default link buildfileRawString         String
highlight default link buildfileMultiString       String
highlight default link buildfileRegex             String

highlight default link buildfileNumber            Number
highlight default link buildfileBoolean           Boolean
highlight default link buildfileNull              Constant

highlight default link buildfileVariable          Identifier
highlight default link buildfileSpecialVar        Special
highlight default link buildfileProperty          Identifier

highlight default link buildfileOperator          Operator
highlight default link buildfileDelimiter         Delimiter
highlight default link buildfileScopeOperator     Operator

highlight default link buildfilePath              Underlined
highlight default link buildfileTarget            Identifier
highlight default link buildfileAttribute         Special

highlight default link buildfileLineContinuation  Special
highlight default link buildfileEscape            SpecialChar

" Restore compatibility options
let &cpo = s:cpo_save
unlet s:cpo_save

let b:current_syntax = "buildfile"

" vim: ts=2 sw=2 et
