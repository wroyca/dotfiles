# Dotfiles

This repository contains my personal dotfiles, managed with
[chezmoi](https://www.chezmoi.io/). They're designed to be portable and easy to
bootstrap, whether we're setting up a long-term system or configuring a
short-lived environment like a container or live session.

The instructions below walk through the setup process.

> [!NOTE]
> The term dotfiles originally referred to configuration files located in a
> user's home directory, typically hidden by a leading period. Over time, the
> meaning has expanded. It now commonly denotes a structured system setup
> maintained under version control. In this context, my dotfiles also include
> package definitions and system-level adjustments, among others.

## Install chezmoi and apply my dotfiles

For most use cases, this is the standard setup. It installs chezmoi using
`chezmoi` bootstrap script and immediately applies dotfiles:

```bash
sh -c "$(curl -fsLS get.chezmoi.io/lb)" -- init --apply wroyca
```

After some times, the system should reflect the configuration defined in this
repository.

## One-shot setup for temporary environments

If we're working in a transient or disposable environment, such as a container
or ephemeral VM, it can be useful to apply dotfiles *without* leaving chezmoi
installed afterward.

This command installs chezmoi, applies dotfiles once, and then removes all
traces of chezmoi, including its source and configuration directories:

```bash
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --one-shot wroyca
```

## Keep our setup up to date

Once chezmoi is installed, keeping things current is straightforward. We can
pull the latest changes and reapply them with:

```bash
chezmoi update
```

Under the hood, this runs a `git pull --autostash --rebase` in the source
directory, followed by a `chezmoi apply`.

## Preview changes before applying them

To review what would change without making any modifications:

```bash
chezmoi git pull -- --autostash --rebase && chezmoi diff
```

This shows a diff between the system's current state and the target state
defined in the repository. Nothing is applied unless we explicitly run:

```bash
chezmoi apply
```
