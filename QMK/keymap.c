#include QMK_KEYBOARD_H

#include "version.h"
#include "achordion.h"

enum custom_keycodes {
  RGB_SLD = EZ_SAFE_RANGE,
};

enum tap_dance_codes {
  DANCE_0,
};

// https://colemakmods.github.io/mod-dh/
//
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [0] = LAYOUT_ergodox_pretty(
    KC_ESCAPE,      KC_1,           KC_2,           KC_3,           KC_4,           KC_5,           KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_6,           KC_7,           KC_8,           KC_9,           KC_0,           KC_DELETE,
    KC_TAB,         KC_Q,           KC_W,           KC_F,           KC_P,           KC_B,           KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_J,           KC_L,           KC_U,           KC_Y,           KC_SCLN,        KC_TRANSPARENT,
    KC_TRANSPARENT, MT(MOD_LSFT, KC_A),MT(MOD_LCTL, KC_R),MT(MOD_LALT, KC_S),LT(1,KC_T),     KC_G,                                                                           KC_M,           LT(1,KC_N),     MT(MOD_RALT, KC_E),MT(MOD_RCTL, KC_I),MT(MOD_RSFT, KC_O),KC_TRANSPARENT,
    KC_TRANSPARENT, KC_Z,           KC_X,           KC_C,           KC_D,           KC_V,           KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_K,           KC_H,           KC_COMMA,       KC_DOT,         KC_SLASH,       KC_TRANSPARENT,
    KC_TRANSPARENT, KC_LEFT_GUI,    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                                                                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_RIGHT_GUI,   KC_TRANSPARENT,
                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, TD(DANCE_0),
                                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                    KC_SPACE,       LT(2,KC_ENTER), KC_TRANSPARENT, KC_TRANSPARENT, LT(2,KC_ENTER), KC_BSPC
  ),

  [1] = LAYOUT_ergodox_pretty(
    KC_TRANSPARENT, KC_EXLM,        KC_AT,          KC_HASH,        KC_DLR,         KC_PERC,        KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_CIRC,        KC_AMPR,        KC_ASTR,        KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
    KC_TRANSPARENT, KC_TILD,        KC_TRANSPARENT, KC_LCBR,        KC_RCBR,        KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_UNDS,        KC_PLUS,        KC_PIPE,        KC_DQUO,        KC_TRANSPARENT,
    KC_TRANSPARENT, KC_GRAVE,       KC_TRANSPARENT, KC_LPRN,        KC_RPRN,        KC_TRANSPARENT,                                                                 KC_TRANSPARENT, KC_MINUS,       KC_EQUAL,       KC_BSLS,        KC_QUOTE,       KC_TRANSPARENT,
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_LBRC,        KC_RBRC,        KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                                                                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                                    QK_BOOT,        KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT
  ),

  [2] = LAYOUT_ergodox_pretty(
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                                                 KC_TRANSPARENT, KC_LEFT,        KC_DOWN,        KC_UP,          KC_RIGHT,       KC_TRANSPARENT,
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                                                                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT
  ),
};

typedef struct {
    uint8_t step;
} TapDanceState;

typedef enum {
    SINGLE_TAP = 1,
    SINGLE_HOLD,
    DOUBLE_TAP,
    DOUBLE_HOLD,
    DOUBLE_SINGLE_TAP,
    MORE_TAPS
} TapDanceStep;

static TapDanceState dance_state[1];

static TapDanceStep
evaluate_tap_dance_step (tap_dance_state_t *state)
{
  if (state->count == 1)
    return (state->interrupted || !state->pressed) ? SINGLE_TAP : SINGLE_HOLD;

  if (state->count == 2)
    {
      if (state->interrupted)
        return DOUBLE_SINGLE_TAP;
      return state->pressed ? DOUBLE_HOLD : DOUBLE_TAP;
    }

  return MORE_TAPS;
}

static void
execute_tap_dance_action (TapDanceStep step)
{
  switch (step)
    {
      case SINGLE_TAP:
        register_code16 (LSFT (KC_PSCR));
        break;
      case SINGLE_HOLD:
        register_code16 (KC_PSCR);
        break;
      case DOUBLE_TAP:
        register_code16 (LALT (KC_PSCR));
        break;
      case DOUBLE_SINGLE_TAP:
        tap_code16 (LSFT (KC_PSCR));
        register_code16 (LSFT (KC_PSCR));
        break;
      default:
        break;
    }
}

static void
reset_tap_dance_action (TapDanceStep step)
{
  wait_ms(10);

  switch (step)
    {
      case SINGLE_TAP:
        unregister_code16 (LSFT (KC_PSCR));
        break;
      case SINGLE_HOLD:
        unregister_code16 (KC_PSCR);
        break;
      case DOUBLE_TAP:
        unregister_code16 (LALT (KC_PSCR));
        break;
      case DOUBLE_SINGLE_TAP:
        unregister_code16 (LSFT (KC_PSCR));
        break;
      default:
        break;
    }
}

void
on_tap_dance_start (tap_dance_state_t *state, void *user_data)
{
  if (state->count >= 3)
    tap_code16 (LSFT (KC_PSCR));
}

void
on_tap_dance_finish (tap_dance_state_t *state, void *user_data)
{
  TapDanceStep step = evaluate_tap_dance_step (state);

  dance_state[0].step = step;

  execute_tap_dance_action (step);
}

void
on_tap_dance_reset (tap_dance_state_t *state, void *user_data)
{
  reset_tap_dance_action (dance_state[0].step);

  dance_state[0].step = 0;
}

tap_dance_action_t tap_dance_actions[] = {
  [DANCE_0] = ACTION_TAP_DANCE_FN_ADVANCED (on_tap_dance_start, on_tap_dance_finish, on_tap_dance_reset),
};

bool
process_record_user (uint16_t keycode, keyrecord_t* record)
{
  if (!process_achordion (keycode, record))
      return false;

  return true;
}

bool 
achordion_chord (uint16_t, keyrecord_t*, uint16_t, keyrecord_t*) 
{
  // We ideally want to make exceptions for Ctrl chord, but I wasn't able to get
  // it working on the left-hand side, even though it does function on the
  // right-hand side. We should investigate and debug this at some point, but for
  // now, let's disable bilateral combinations.
  //
  return true;
}

void
matrix_scan_user (void)
{
  achordion_task ();
}

uint8_t
layer_state_set_user (uint8_t state)
{
  uint8_t layer = biton (state);

  ergodox_board_led_off ();

  ergodox_right_led_1_off ();
  ergodox_right_led_2_off ();
  ergodox_right_led_3_off ();

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
