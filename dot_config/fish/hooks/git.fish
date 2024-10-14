# Shorthand for cloning GitHub repositories with `git clone foo/bar.git`, where
# `foo` is the username and `bar` is the repository name.
#
function git
  if test "x$argv[1]" = "xclone" -a (string match -r "^x[^/]+/[^/]+\.git\$" "x$argv[2]")
    command git clone "https://github.com/$argv[2]"
  else
    command git $argv
  end
end
