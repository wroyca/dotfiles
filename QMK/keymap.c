#include QMK_KEYBOARD_H

#include "version.h"

enum custom_keycodes
{
  RGB_SLD = EZ_SAFE_RANGE,
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  // https://colemakmods.github.io/mod-dh/
  //
  [0] = LAYOUT_ergodox_pretty(
    KC_ESCAPE,      KC_1,           KC_2,           KC_3,           KC_4,           KC_5,           KC_LBRC,                                        KC_RBRC,        KC_6,           KC_7,           KC_8,           KC_9,           KC_0,           KC_ESCAPE,
    KC_TAB,         KC_Q,           KC_W,           KC_F,           KC_P,           KC_B,           KC_LPRN,                                        KC_RPRN,        KC_J,           KC_L,           KC_U,           KC_Y,           KC_SCLN,        KC_TAB,
    MO(1),          KC_A,           KC_R,           KC_S,           KC_T,           KC_G,                                                                           KC_M,           KC_N,           KC_E,           KC_I,           KC_O,           MO(1),
    KC_LEFT_SHIFT,  KC_Z,           KC_X,           KC_C,           KC_D,           KC_V,           KC_LCBR,                                        KC_RCBR,        KC_K,           KC_H,           KC_COMMA,       KC_DOT,         KC_SLASH,       KC_RIGHT_SHIFT,
    KC_LEFT_CTRL,   KC_LEFT_GUI,    KC_LEFT_ALT,    KC_TRANSPARENT, KC_LEFT,                                                                                                        KC_RIGHT,       KC_TRANSPARENT, KC_RIGHT_ALT,   KC_RIGHT_GUI,   KC_RIGHT_CTRL,
                                                                                                    QK_BOOT,        KC_TRANSPARENT, KC_TRANSPARENT, KC_PSCR,
                                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                    KC_SPACE,       KC_ENTER,       KC_TRANSPARENT, KC_TRANSPARENT, KC_ENTER,       KC_BSPC
  ),

  [1] = LAYOUT_ergodox_pretty(
    KC_ESCAPE,      KC_F1,          KC_F2,          KC_F3,          KC_F4,          KC_F5,          KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_F6,          KC_F7,          KC_F8,          KC_F9,          KC_F10,         KC_F11,
    KC_TRANSPARENT, KC_EXLM,        KC_AT,          KC_LCBR,        KC_RCBR,        KC_PIPE,        KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_UP,          KC_7,           KC_8,           KC_9,           KC_ASTR,        KC_F12,
    KC_TRANSPARENT, KC_HASH,        KC_DLR,         KC_LPRN,        KC_RPRN,        KC_GRAVE,                                                                       KC_DOWN,        KC_4,           KC_5,           KC_6,           KC_PLUS,        KC_TRANSPARENT,
    KC_TRANSPARENT, KC_PERC,        KC_CIRC,        KC_LBRC,        KC_RBRC,        KC_TILD,        KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_AMPR,        KC_1,           KC_2,           KC_3,           KC_BSLS,        KC_TRANSPARENT,
    KC_TRANSPARENT, KC_EQUAL,       KC_TRANSPARENT, KC_QUOTE,       KC_KP_MINUS,                                                                                                    KC_TRANSPARENT, KC_DOT,         KC_0,           KC_EQUAL,       KC_TRANSPARENT,
                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT
  ),

  [2] = LAYOUT_ergodox_pretty(
    KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,                                          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,
    KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,                                          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,
    KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,                                                                          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,
    KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,                                          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,
    KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,                                                                                                          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,
                                                                                                    KC_NO,          KC_NO,          KC_NO,          KC_NO,
                                                                                                                    KC_NO,          KC_NO,
                                                                                    KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO,          KC_NO
  ),
};

bool
process_record_user (uint16_t keycode, keyrecord_t* record)
{
  return true;
}

uint8_t
layer_state_set_user (uint8_t state)
{
  uint8_t layer = biton (state);

  ergodox_board_led_off ();

  switch (layer)
    {
    case 1:
      ergodox_right_led_1_on ();
      break;
    case 2:
      ergodox_right_led_2_on ();
      break;
    case 3:
      ergodox_right_led_3_on ();
      break;
    case 4:
      ergodox_right_led_1_on ();
      ergodox_right_led_2_on ();
      break;
    case 5:
      ergodox_right_led_1_on ();
      ergodox_right_led_3_on ();
      break;
    case 6:
      ergodox_right_led_2_on ();
      ergodox_right_led_3_on ();
      break;
    case 7:
      ergodox_right_led_1_on ();
      ergodox_right_led_2_on ();
      ergodox_right_led_3_on ();
      break;
    default:
      break;
    }

  return state;
};
