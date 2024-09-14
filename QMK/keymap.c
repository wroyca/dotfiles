#include QMK_KEYBOARD_H

#include "version.h"

enum custom_keycodes
{
  RGB_SLD = EZ_SAFE_RANGE,
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [0] = LAYOUT_ergodox_pretty(
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
