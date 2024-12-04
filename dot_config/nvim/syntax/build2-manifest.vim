" Vim syntax file
" Language:    Build2 (manifest)
" Maintainer:  William Roy <wroy@proton.me>

if exists("b:current_syntax")
  finish
endif

" Special syntax which tells vim to reset cpo which defaults to aABceFs.
let s:cpo_save = &cpo
set cpo&vim

" -----------------------------------------------------------------------------
" Escape
" -----------------------------------------------------------------------------
syntax match buildfileEscape "\\\n\s*"

" -----------------------------------------------------------------------------
" Comments
" -----------------------------------------------------------------------------
syn region manifestCommentLine   start="^\s*#" end="$"  contains=@Spell nextgroup=buildfileEscape skipempty
syn region manifestCommentEscape start=/\\\n/  end=/\n/ contains=ALLBUT,manifestCommentLine

" -----------------------------------------------------------------------------
" Identifier
" -----------------------------------------------------------------------------
syntax match manifestIndentifier /name:/
syntax match manifestIndentifier /version:/
syntax match manifestIndentifier /upstream-version:/
syntax match manifestIndentifier /type:/
syntax match manifestIndentifier /language:/
syntax match manifestIndentifier /project:/
syntax match manifestIndentifier /priority:/
syntax match manifestIndentifier /summary:/
syntax match manifestIndentifier /license:/
syntax match manifestIndentifier /topics:/
syntax match manifestIndentifier /keywords:/
syntax match manifestIndentifier /description:/
syntax match manifestIndentifier /description-file:/
syntax match manifestIndentifier /description-type:/
syntax match manifestIndentifier /package-description:/
syntax match manifestIndentifier /package-description-file:/
syntax match manifestIndentifier /package-description-type:/
syntax match manifestIndentifier /changes:/
syntax match manifestIndentifier /changes-file:/
syntax match manifestIndentifier /changes-type:/
syntax match manifestIndentifier /url:/
syntax match manifestIndentifier /doc-url:/
syntax match manifestIndentifier /src-url:/
syntax match manifestIndentifier /package-url:/
syntax match manifestIndentifier /email:/
syntax match manifestIndentifier /package-email:/
syntax match manifestIndentifier /build-email:/
syntax match manifestIndentifier /build-warning-email:/
syntax match manifestIndentifier /build-error-email:/
syntax match manifestIndentifier /depends:/
syntax match manifestIndentifier /requires:/
syntax match manifestIndentifier /tests:/
syntax match manifestIndentifier /examples:/
syntax match manifestIndentifier /benchmarks:/
syntax match manifestIndentifier /builds:/
syntax match manifestIndentifier /build-include:/
syntax match manifestIndentifier /build-exclude:/
syntax match manifestIndentifier /build-auxiliary:/
syntax match manifestIndentifier /build-auxiliary-*:/
syntax match manifestIndentifier /build-bot:/
syntax match manifestIndentifier /*-build-config:/
syntax match manifestIndentifier /*-builds:/
syntax match manifestIndentifier /*-build-include:/
syntax match manifestIndentifier /*-build-exclude:/
syntax match manifestIndentifier /*-build-auxiliary:/
syntax match manifestIndentifier /*-build-auxiliary-*:/
syntax match manifestIndentifier /*-build-bot:/
syntax match manifestIndentifier /*-build-email:/
syntax match manifestIndentifier /*-build-warning-email:/
syntax match manifestIndentifier /*-build-error-email:/
syntax match manifestIndentifier /build-file:/
syntax match manifestIndentifier /bootstrap-build:/
syntax match manifestIndentifier /root-build:/
syntax match manifestIndentifier /*-build:/
syntax match manifestIndentifier /bootstrap-build2:/
syntax match manifestIndentifier /root-build2:/
syntax match manifestIndentifier /*-build2:/
syntax match manifestIndentifier /*-name:/
syntax match manifestIndentifier /*-version:/
syntax match manifestIndentifier /*-to-downstream-version:/

" -----------------------------------------------------------------------------
" HL Link
" -----------------------------------------------------------------------------
highlight link manifestIndentifier   Identifier
highlight link manifestCommentLine   Comment
highlight link manifestCommentEscape Normal
