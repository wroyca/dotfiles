function fish_prompt
  set -l cwd (pwd | string replace "$HOME" '~')
  set -l git (echo (__fish_git_prompt) | sed -e 's/((/(/; s/))/)/')

  if [ -f /run/.containerenv ]
    echo -n (set_color magenta)"⬢"(set_color normal)
  end

  echo -n '['
    echo -n (whoami)'@'(hostname) (basename "$cwd")
    echo -n $git
  echo -n ']$ '
end

function fish_greeting
end

fish_add_path /home/wroy/.config/emacs/bin
fish_add_path /home/wroy/.cargo/bin
fish_add_path /home/wroy/.dotnet/tools
fish_add_path /home/wroy/.local/bin

# Bash aliases are compatible with Fish.
#
source ~/.bashrc.d/bash_aliases

# Register shell environment
#
export SHELL=fish
