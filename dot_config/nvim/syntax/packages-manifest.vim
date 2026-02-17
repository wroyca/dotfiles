" Vim syntax file
" Language:     build2 package list manifest (packages.manifest)
" Maintainer:   William Roy <wroy@proton.me>
" Filenames:    packages.manifest
" URL:          https://build2.org
"
" References:
"   - build2 Package Manager Manual: https://build2.org/bpkg/doc/build2-package-manager-manual.xhtml#manifest-package-list

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

" =============================================================================
" Comments
" =============================================================================

" Line comments starting with #
syntax match packagesManifestComment /^\s*#.*$/

" =============================================================================
" Manifest Format Version
" =============================================================================

" Format: : <version>
syntax match packagesManifestVersion /^\s*:\s*\d\+\s*$/

" =============================================================================
" Common Fields
" =============================================================================

" Field names (location, sha256sum, fragment)
syntax match packagesManifestField /^\s*location\s*:/
syntax match packagesManifestField /^\s*sha256sum\s*:/
syntax match packagesManifestField /^\s*fragment\s*:/


" =============================================================================
" Highlight Linking
" =============================================================================

highlight default link packagesManifestComment         Comment
highlight default link packagesManifestVersion         PreProc
highlight default link packagesManifestField           Identifier

" Restore compatibility options
let b:current_syntax = "packages-manifest"

let &cpo = s:cpo_save
unlet s:cpo_save
