" Vim syntax file
" Language:     build2 repository manifest (repositories.manifest)
" Maintainer:   William Roy <wroy@proton.me>
" Filenames:    repositories.manifest
" URL:          https://build2.org
"
" References:
"   - build2 Package Manager Manual: https://build2.org/bpkg/doc/build2-package-manager-manual.xhtml#manifest-repository

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

" =============================================================================
" Comments
" =============================================================================

" Line comments starting with #
syntax match repositoriesManifestComment /^\s*#.*$/

" Inline comments after semicolon (part of the value, serve as documentation)
syntax match repositoriesManifestInlineComment /;.*$/ contained

" =============================================================================
" Manifest Format Version
" =============================================================================

" Format: : <version>
syntax match repositoriesManifestVersion /^\s*:\s*\d\+\s*$/

" =============================================================================
" Common Fields
" =============================================================================

" Field names
syntax match repositoriesManifestField /^\s*location\s*:/
syntax match repositoriesManifestField /^\s*type\s*:/
syntax match repositoriesManifestField /^\s*role\s*:/
syntax match repositoriesManifestField /^\s*trust\s*:/
syntax match repositoriesManifestField /^\s*url\s*:/
syntax match repositoriesManifestField /^\s*email\s*:/
syntax match repositoriesManifestField /^\s*summary\s*:/
syntax match repositoriesManifestField /^\s*description\s*:/
syntax match repositoriesManifestField /^\s*certificate\s*:/
syntax match repositoriesManifestField /^\s*fragment\s*:/

" =============================================================================
" Highlight Linking
" =============================================================================

highlight default link repositoriesManifestComment         Comment
highlight default link repositoriesManifestInlineComment   Comment
highlight default link repositoriesManifestVersion         PreProc
highlight default link repositoriesManifestField           Identifier

" Restore compatibility options
let b:current_syntax = "repositories-manifest"

let &cpo = s:cpo_save
unlet s:cpo_save
