# https://docs.qmk.fm/#/config_options?id=the-rulesmk-file

COMMAND_ENABLE = no
CONSOLE_ENABLE = no
LTO_ENABLE = yes
MOUSEKEY_ENABLE = no
ORYX_ENABLE = yes
RGBLIGHT_ENABLE = no
SPACE_CADET_ENABLE = no
TAP_DANCE_ENABLE = yes

# Achordion is a userspace QMK library that customizes when tap-hold keys are
# considered held vs. tapped based on the next pressed key. The library works
# on top of QMK’s existing tap-hold implementation.
#
# https://getreuer.info/posts/keyboards/achordion/index.html
#
SRC += achordion.c
