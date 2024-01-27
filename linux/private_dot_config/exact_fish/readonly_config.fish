source ~/.config/fish/fish_aliases.fish

# Supress the greeting message printed on startup.
#
function fish_greeting
end

# Unlike other shells, there is no prompt variable like PS1. To display our
# prompt, fish executes the fish_prompt function and uses its output as the
# prompt. And if it exists, fish also executes the fish_right_prompt function
# and uses its output as the right prompt.
#
function fish_prompt
  set -l cwd (pwd | string replace "$HOME" '~')
  set -l git_branch (git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/')

  echo -n '['(whoami)'@'(hostname) (basename "$cwd")"$git_branch"']$ '
end

# {{{ Binding. z-o to expand.

bind \cz 'fg 2>/dev/null; commandline -f repaint'

# }}}

