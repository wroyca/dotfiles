source ~/.config/fish/fish_aliases.fish

# Unlike other shells, there is no prompt variable like PS1. To display our
# prompt, fish executes the fish_prompt function and uses its output as the
# prompt. And if it exists, fish also executes the fish_right_prompt function
# and uses its output as the right prompt.
#
function fish_prompt
  set -l cwd (pwd | string replace "$HOME" '~')
  set -l branch (git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/')
  
  # Show an icon when inside a container, such as a toolbox or distrobox.
  #
  if test -f /run/.containerenv
    echo -n (set_color magenta)⬢(set_color normal)
  end
  
  # [user@hostname ~]$ ...
  echo -n '['
    echo -n (whoami)'@'(hostname) (basename "$cwd")"$git_branch"
  echo -n ']$ '
end


# Suppress greeting message printed on startup.
#
function fish_greeting 
end
