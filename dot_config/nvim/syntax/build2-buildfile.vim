" Vim syntax file
" Language:    Build2 (buildfile)
" Maintainer:  William Roy <wroy@proton.me>

if exists("b:current_syntax")
  finish
endif

" Special syntax which tells vim to reset cpo which defaults to aABceFs.
let s:cpo_save = &cpo
set cpo&vim

" -----------------------------------------------------------------------------
" Builtin
" -----------------------------------------------------------------------------
syntax keyword buildfileBuiltin cat
syntax keyword buildfileBuiltin cp
syntax keyword buildfileBuiltin date
syntax keyword buildfileBuiltin diff
syntax keyword buildfileBuiltin echo
syntax keyword buildfileBuiltin env
syntax keyword buildfileBuiltin exit
syntax keyword buildfileBuiltin ln
syntax keyword buildfileBuiltin mkdir
syntax keyword buildfileBuiltin mv
syntax keyword buildfileBuiltin rm
syntax keyword buildfileBuiltin rmdir
syntax keyword buildfileBuiltin sed
syntax keyword buildfileBuiltin set
syntax keyword buildfileBuiltin sleep
syntax keyword buildfileBuiltin test
syntax keyword buildfileBuiltin touch
syntax keyword buildfileBuiltin info
syntax keyword buildfileBuiltin text
syntax keyword buildfileBuiltin warn
syntax keyword buildfileBuiltin fail
syntax keyword buildfileBuiltin print
syntax keyword buildfileBuiltin assert
syntax keyword buildfileBuiltin dump

" -----------------------------------------------------------------------------
" Escape
" -----------------------------------------------------------------------------
syntax match buildfileEscape "\\\n\s*"

" -----------------------------------------------------------------------------
" Comments
" -----------------------------------------------------------------------------
syn region buildfileCommentLine                                   start="#"             end="$"          contains=@Spell
syn region buildfileCommentBlock matchgroup=buildfileCommentBlock start="/^\s*#\ze\\$/" end="/^\s*#\\$/" contains=@Spell

" -----------------------------------------------------------------------------
" String Literals
" -----------------------------------------------------------------------------
syn region buildfileString        start=/"/  end=/"/ oneline
syn region buildfileString        start=/'/  end=/'/ oneline
syn region buildfileSpecialString start=/\~"/ end=/"/ oneline
syn region buildfileSpecialString start=/\~'/ end=/'/ oneline

" -----------------------------------------------------------------------------
" Identifier
" -----------------------------------------------------------------------------
syntax match buildfileIndentifier /^\s*\v(project|extension|backlink)\>/
syntax match buildfileIndentifier /\v(config\.)?(cc|cxx|c)(\.export)?(\.(poptions|coptions|loptions|aoptions|libs))/
syntax match buildfileIndentifier /^\s*\v[a-zA-Z][a-zA-Z0-9_.]*\>/

" -----------------------------------------------------------------------------
" PreProc
" -----------------------------------------------------------------------------
syntax match buildfilePreProc /\$[a-zA-Z][a-zA-Z0-9_.]*/
syntax match buildfilePreProc /\$\([a-zA-Z][a-zA-Z0-9_.]*\)/
syntax match buildfilePreProc /(\$)(\(\)/
syntax match buildfilePreProc /\$\([<>]\)/

" -----------------------------------------------------------------------------
" Type
" -----------------------------------------------------------------------------
syntax match buildfileType /\v\[\w+\]/
syntax match buildfileType "/\v(bool\|uint64\|uint64s|string|strings|path\|paths|dir_path\|dir_paths|name\|names|name_pair\|project_name\|target_triplet)/"
syntax match buildfileType /@\v/

" -----------------------------------------------------------------------------
" Function
" -----------------------------------------------------------------------------
syntax match  buildfileFunction /\$[a-zA-Z][a-zA-Z0-9_.]*\ze(/ contains=buildfileFunctionArgs
syntax region buildfileFunctionArgs start="*\ze(\([^)]*\))/" end="/(\zs[^)\n]*)/" contained

" -----------------------------------------------------------------------------
" HL Link
" -----------------------------------------------------------------------------
highlight link buildfileEscape                Special
highlight link buildfileCommentLine           Comment
highlight link buildfileCommentBlock          Comment
highlight link buildfileString                String
highlight link buildfileSpecialString         SpecialChar
highlight link buildfileIndentifier           Identifier
highlight link buildfilePreProc               PreProc
highlight link buildfileType                  Type
highlight link buildfileFunction              Function
highlight link buildfileFunctionArgs          Function
highlight link buildfileKeyword               Keyword
highlight link buildfileBuiltin               Function
