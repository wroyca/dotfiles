function fish_prompt
  set -l cwd (pwd | string replace "$HOME" '~')
  set -l branch (git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/')

  # [user@hostname ~]$ ...
  #
  echo -n '['
    echo -n (whoami)'@'(hostname) (basename "$cwd")"$branch"
  echo -n ']$ '
end

function fish_greeting
end

# Source bash aliases, as simpler ones are compatible with Fish.
#
source ~/.config/fish/fish_aliases.fish

# Bind `ctrl-z` to resume suspended jobs in the background and foreground.
#
bind \cz 'fg 2>/dev/null; commandline -f repaint'

# User specific environment and startup programs
#
fish_add_path /home/wroy/bin
fish_add_path /home/wroy/cargo/bin
fish_add_path /home/wroy/.dotnet/tools
