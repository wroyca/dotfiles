#!/usr/bin/env fish

# FIXME:
#
# https://github.com/fish-shell/fish-shell/issues/5186
# https://github.com/fish-shell/fish-shell/issues/6124

# The build2 project dependency manager is used to manage the dependencies of a
# project during development. This function intercepts specific `bdep`
# commands to handle special cases with an emphasis on ergonomics.
#
function bdep
  set -l argv_length (count $argv)

  # Intercepts default configuration change.
  #
  # When setting a new default configuration with bdep, you must first remove
  # the existing default configuration. If you attempt to set a new default
  # while one is already in place, bdep will return an error message, such as:
  # error: configuration @gcc of type target is already the default.
  #
  # In essence, setting a new default implies changing from one default to
  # another. Therefore, before applying a new default configuration, we ensure
  # that the previous default is automatically (and properly) unset.
  #
  # https://github.com/build2/build2/issues/131
  #
  if test "x$argv[1]" = "xconfig" -a "x$argv[2]" = "xset" -a "x$argv[3]" = "x--default"
    bdep config set --no-default (bdep config list | grep 'default' | awk '{print $1}') --quiet

    command bdep $argv --quiet
    command bdep  sync --quiet # Explicit sync is required for changes to take effect.
    return
  end

  # Intercepts the init command for existing configurations.
  #
  # Normally, to override an existing configuration, a multi-step process is
  # required: first, you must run bdep config remove, which then prompts you to
  # execute bdep deinit to deinitialize the configuration. After that, you'll
  # need to run bdep config remove once more to actually remove the
  # configuration.
  #
  # This multi-step approach is not only inefficient but also quite tedious.
  # Instead, the process can be streamlined by prompting for confirmation to
  # proceed with the override. Once confirmed, it automatically handles the
  # deinitialization and removal of the existing configuration in one step, and
  # then re-executes the bdep init -C command with the provided arguments.
  #
  if test "x$argv[1]" = "xinit"
    set -l config_name
    set -l config_flag 0

    for i in (seq 2 $argv_length)
      if test "x$argv[$i]" = "x-C"
        set config_flag 1
        continue
      end

      if test $config_flag -eq 1
        if not string match -rq -- "--" $argv[$i]
          set config_name $argv[$i]
          break
        end
      end
    end

    if test -z "$config_name"
      echo "No configuration name found after -C."
      return 1
    end

    set exists (bdep config list | grep $config_name)

    if test -n "$exists"
      echo "configuration $config_name already exists."
      read -P "override? [y/N] " confirm
      if test "x$confirm" = "xy" -o "x$confirm" = "xY"
        bdep deinit $config_name
        bdep config remove $config_name
      else
        return 1
      end
    end

    # Use --wipe to clean the existing configuration.
    #
    # Note that this is necessary since build2 does not remove the config
    # directory when running `config remove`.
    #
    command bdep $argv --wipe
    return
  end

  # Pass all other commands through to the original bdep
  command bdep $argv
end
