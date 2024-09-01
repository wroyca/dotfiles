# ~/.dotfiles

### Install chezmoi and the dotfiles on a new machine.

```bash
sh -c "$(curl -fsLS get.chezmoi.io/lb)" -- init --apply wroyca --branch ergodox
```

### Install chezmoi and the dotfiles in a transitory environments (e.g. short-lived Linux containers).

This install the dotfiles, and then remove all traces of chezmoi, including the
source directory and chezmoi's configuration directory.

```bash
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --one-shot wroyca --branch ergodox
```

### Pull the latest changes from the repo and apply them.

This runs `git pull --autostash --rebase` in the source directory and then
`chezmoi apply`.

```bash
chezmoi update
```

### Pull the latest changes from the repo and see what would change, without actually applying the changes.

This runs `git pull --autostash --rebase` in the source directory and
`chezmoi diff` then shows the difference between the target state computed
from the source directory and the actual state.

```bash
chezmoi git pull -- --autostash --rebase && chezmoi diff
```


