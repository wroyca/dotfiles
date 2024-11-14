// https://docs.qmk.fm/#/config_options?id=the-configh-file

#define LAYER_STATE_8BIT
#define PERMISSIVE_HOLD
#define SERIAL_NUMBER "RmGxP/0gPx6"
#define USB_SUSPEND_WAKEUP_DELAY 0

// The ACHORDION_STREAK option disables hold behavior during a typing streak.
//
// In a sequence of key presses like A, B, C—where B is a tap-and-hold key and
// A and C are regular keys—a "streak" is defined as the duration between the
// release of A and the press of C, provided it happens within a time frame
// shorter than the default timeout of 200 ms.
//
#define ACHORDION_STREAK
