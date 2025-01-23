" Vim compiler file
" Compiler: build2

if exists('b:current_compiler')
  finish
endif

let b:current_compiler = 'b'

let s:save_cpoptions = &cpoptions
set cpoptions&vim

CompilerSet makeprg=b

let &cpoptions = s:save_cpoptions
unlet s:save_cpoptions
