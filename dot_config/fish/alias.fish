# We want to make sure we don't shoot ourselves in the foot by accidentally
# operating on the root directory recursively, which is a surprisingly easy
# mistake to make. So we are going to enforce the --preserve-root option for
# these critical coreutils.
#
alias chgrp='chgrp --preserve-root'
alias chmod='chmod --preserve-root'
alias chown='chown --preserve-root'

# Similarly, for file manipulation we prefer to be prompted before overwriting
# existing files. The idea here is that the slight annoyance of having to
# confirm an overwrite is well worth the safety. We also make them verbose so
# we can see exactly what is being done (which is also why we add -pv to mkdir
# to see the intermediate directories being created).
#
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'
alias mkdir='mkdir -pv'

# For disk and memory usage diagnostics, the default byte counts are not
# particularly easy to read at a glance. So we pass the -h flag to get
# human-readable sizes (megabytes, gigabytes, etc).
#
alias df='df -h'
alias du='du -h'
alias free='free -h'

# The default diff output format is archaic and quite difficult to read.
# The unified format is the standard for modern development (it is exactly
# what Git uses under the hood), so we default to that. 
#
alias diff='diff -u'

# When inspecting directory structures, certain auxiliary directories usually just add a lot of visual
# noise that we don't really care about. So we exclude them and ask tree
# to list files first for a more logical visual grouping.
#
alias tree='tree --filesfirst -a -I ".git|.cache|.bdep"'
alias tree='tree --filesfirst -a -I ".git|.cache|.bdep"'

# Note that the standard clear command often just scrolls the terminal down
# without actually clearing the scrollback buffer. We use these specific ANSI
# escape sequences to completely wipe the screen AND the scrollback buffer, placing the
# cursor back at the top left.
#
alias clear "printf '\033[2J\033[3J\033[1;1H'"
