# Configure environment to use Starship in Nushell.

export-env {
  # Explicitly set the shell name for Starship.
  #
  $env.STARSHIP_SHELL = "nu"

  load-env {
    # Generate a unique session key to allow Starship to distinguish
    # between shell sessions. This helps enable session-specific
    # behaviors such as caching or telemetry separation.
    #
    STARSHIP_SESSION_KEY: (random chars -l 16)

    # Override the default multiline prompt indicator. Note that the
    # default character module may interfere with visual layout, hence
    # we provide our own continuation prompt via Starship.
    #
    PROMPT_MULTILINE_INDICATOR: (
      ^/home/wroy/.local/bin/starship prompt --continuation
    )

    # The default prompt indicator is disabled since we delegate full
    # prompt rendering to Starship (see PROMPT_COMMAND below).
    #
    PROMPT_INDICATOR: ""

    # Configure the main prompt command.
    #
    # This function is invoked by Nushell to generate the primary prompt
    # line. We call Starship directly with relevant context:
    #
    # - The duration of the last executed command (CMD_DURATION_MS),
    # - The exit status of the previous command (LAST_EXIT_CODE),
    # - The current terminal width, required for correct alignment.
    #
    # Note: Starship's job module is disabled since Nushell does not yet
    # support background job tracking.
    #
    PROMPT_COMMAND: {||
      (
        ^/home/wroy/.local/bin/starship prompt
          --cmd-duration $env.CMD_DURATION_MS
          $"--status=($env.LAST_EXIT_CODE)"
          --terminal-width (term size).columns
      )
    }

    # Merge additional configuration into the Nushell runtime.
    #
    # We enable right prompt rendering on the final prompt line to
    # ensure consistent layout across all shells (especially useful in
    # wide terminals).
    #
    config: (
      $env.config? | default {} | merge {
        render_right_prompt_on_last_line: true
      }
    )

    # Configure the right-aligned prompt.
    #
    # This is rendered using the same logic and environment as
    # PROMPT_COMMAND, except it includes the `--right` flag to request
    # right-aligned layout.
    #
    PROMPT_COMMAND_RIGHT: {||
      (
        ^/home/wroy/.local/bin/starship prompt
          --right
          --cmd-duration $env.CMD_DURATION_MS
          $"--status=($env.LAST_EXIT_CODE)"
          --terminal-width (term size).columns
      )
    }
  }
}
