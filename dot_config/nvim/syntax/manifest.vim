" Vim syntax file
" Language:     build2 package manifest
" Maintainer:   William Roy <wroy@proton.me>
" Filenames:    manifest
" URL:          https://build2.org
"
" References:
"   - build2 Package Manager Manual: https://build2.org/bpkg/doc/build2-package-manager-manual.xhtml#manifest-format

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

" =============================================================================
" Comments
" =============================================================================

" Line comments starting with #
syntax match manifestComment /^\s*#.*$/

" Format: ; <comment>
syntax match manifestInlineComment /;.*$/ contained

" =============================================================================
" Manifest Format Version
" =============================================================================

syntax match manifestVersion /^\s*:\s*\d\+\s*$/

" =============================================================================
" Manifest Fields
" =============================================================================

" Core package metadata
syntax match manifestField /^\s*name\s*:/
syntax match manifestField /^\s*version\s*:/
syntax match manifestField /^\s*upstream-version\s*:/
syntax match manifestField /^\s*type\s*:/
syntax match manifestField /^\s*language\s*:/
syntax match manifestField /^\s*project\s*:/
syntax match manifestField /^\s*priority\s*:/
syntax match manifestField /^\s*summary\s*:/
syntax match manifestField /^\s*license\s*:/
syntax match manifestField /^\s*topics\s*:/
syntax match manifestField /^\s*keywords\s*:/

" Description fields
syntax match manifestField /^\s*description\s*:/
syntax match manifestField /^\s*description-file\s*:/
syntax match manifestField /^\s*description-type\s*:/
syntax match manifestField /^\s*package-description\s*:/
syntax match manifestField /^\s*package-description-file\s*:/
syntax match manifestField /^\s*package-description-type\s*:/
syntax match manifestField /^\s*changes\s*:/
syntax match manifestField /^\s*changes-file\s*:/
syntax match manifestField /^\s*changes-type\s*:/

" URL fields
syntax match manifestField /^\s*url\s*:/
syntax match manifestField /^\s*doc-url\s*:/
syntax match manifestField /^\s*src-url\s*:/
syntax match manifestField /^\s*package-url\s*:/

" Email fields
syntax match manifestField /^\s*email\s*:/
syntax match manifestField /^\s*package-email\s*:/
syntax match manifestField /^\s*build-email\s*:/
syntax match manifestField /^\s*build-warning-email\s*:/
syntax match manifestField /^\s*build-error-email\s*:/

" Dependencies and requirements
syntax match manifestField /^\s*depends\s*:/
syntax match manifestField /^\s*requires\s*:/
syntax match manifestField /^\s*tests\s*:/
syntax match manifestField /^\s*examples\s*:/
syntax match manifestField /^\s*benchmarks\s*:/

" Build configuration fields
syntax match manifestField /^\s*builds\s*:/
syntax match manifestField /^\s*build-include\s*:/
syntax match manifestField /^\s*build-exclude\s*:/
syntax match manifestField /^\s*build-auxiliary\s*:/
syntax match manifestField /^\s*build-bot\s*:/
syntax match manifestField /^\s*build-file\s*:/
syntax match manifestField /^\s*build-auxiliary-[[:alnum:]_-]\+\s*:/

" Package configuration build fields
syntax match manifestField /^\s*[*[:alnum:]_-]\+-build-config\s*:/
syntax match manifestField /^\s*[*[:alnum:]_-]\+-builds\s*:/
syntax match manifestField /^\s*[*[:alnum:]_-]\+-build-include\s*:/
syntax match manifestField /^\s*[*[:alnum:]_-]\+-build-exclude\s*:/
syntax match manifestField /^\s*[*[:alnum:]_-]\+-build-auxiliary\s*:/
syntax match manifestField /^\s*[*[:alnum:]_-]\+-build-bot\s*:/
syntax match manifestField /^\s*[*[:alnum:]_-]\+-build-email\s*:/
syntax match manifestField /^\s*[*[:alnum:]_-]\+-build-warning-email\s*:/
syntax match manifestField /^\s*[*[:alnum:]_-]\+-build-error-email\s*:/
syntax match manifestField /^\s*[*[:alnum:]_-]\+-build-auxiliary-[[:alnum:]_-]\+\s*:/

" Build system skeleton fields
syntax match manifestField /^\s*bootstrap-build\s*:/
syntax match manifestField /^\s*root-build\s*:/
syntax match manifestField /^\s*bootstrap-build2\s*:/
syntax match manifestField /^\s*root-build2\s*:/
syntax match manifestField /^\s*[[:alnum:]_/-]\+-build2\?\s*:/

" Distribution package mapping
syntax match manifestField /^\s*[[:alnum:]_]\+_\?[[:alnum:].]*-name\s*:/
syntax match manifestField /^\s*[[:alnum:]_]\+_\?[[:alnum:].]*-version\s*:/
syntax match manifestField /^\s*[[:alnum:]_]\+_\?[[:alnum:].]*-to-downstream-version\s*:/

" =============================================================================
" Highlight Linking
" =============================================================================

highlight default link manifestVersion            PreProc
highlight default link manifestComment            Comment
highlight default link manifestInlineComment      Comment
highlight default link manifestField              Identifier

" Restore compatibility options
let &cpo = s:cpo_save
unlet s:cpo_save

let b:current_syntax = "manifest"

" vim: ts=2 sw=2 et
