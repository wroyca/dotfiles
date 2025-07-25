// Auto-refresh via <meta http-equiv="refresh"> or the HTTP Refresh header is
// one of those “legacy convenience” features that mostly just gets in the way
// now. If you've ever been filling out a form only to have it wiped
// mid-sentence, you've probably met it.
//
// It would be nice to turn it off entirely, and Firefox gives us that switch,
// but the unfortunate bit is that the same plumbing seems to handle external
// protocol launches. Block this, and suddenly protocol redirect wants a
// confirmation dialog, every time.
//
// So we cave and leave it on. Not because we like it, but because getting
// nagged 30 times a day is worse. Perhaps later we can figure out a way to
// selectively allow only known-safe things (some sort of whitelist or policy
// hook, maybe).
//
user_pref("accessibility.blockautorefresh", false);

// Around Firefox 89, someone decided that what the URL bar really needed
// was tiny clickable buttons on each suggestion row. The result was that
// keyboard navigation started skipping around unpredictably, getting stuck
// on invisible menus you never asked for.
//
// This turns that off. Tabbing through the results works like it used to,
// without second-guessing or surprise detours.
//
user_pref("browser.urlbar.resultMenu.keyboardAccessible", false);

// The new tab page is a festival of sponsored links, algorithmic "top stories,"
// and calls to action for Pocket, which itself is just a sponsored content
// pipeline dressed in personal taste. None of this belongs in a browser, let
// alone in a supposedly clean start page.
//
// These flip every switch we've found that disables Pocket, promotions,
// recommendations, trending topics, highlight cards, and whatever else Mozilla
// is trying to push into that space. It won't stay clean forever, but at least
// we start that way.
//
user_pref("browser.newtabpage.activity-stream.discoverystream.saveToPocketCard.enabled", false);
user_pref("browser.newtabpage.activity-stream.discoverystream.sendToPocket.enabled", false);
user_pref("browser.newtabpage.activity-stream.feeds.section.topstories", false);
user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);
user_pref("browser.newtabpage.activity-stream.section.highlights.includePocket", false);
user_pref("browser.newtabpage.activity-stream.showSponsored", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("extensions.pocket.enabled", false);

// Firefox has never quite looked right under GNOME. The default theme is close
// enough to pass on a screenshot, but not close enough to feel native. This
// pulls in the community GNOME theme, which corrects most of the obvious
// mismatches and makes it feel like something that actually belongs on the
// desktop.
//
// There are also a few layout nudges here: one-tab hiding, bookmark bar
// placement, extension buttons ordered like they were before everything got
// stuffed into a drawer.
//
user_pref("gnomeTheme.hideSingleTab", true);
user_pref("gnomeTheme.bookmarksToolbarUnderTabs", true);
user_pref("browser.theme.dark-private-windows", false);
user_pref("browser.uiCustomization.state", "{\"placements\":{\"widget-overflow-fixed-list\":[],\"unified-extensions-area\":[\"addon_darkreader_org-browser-action\",\"firefoxpwa_filips_si-browser-action\"],\"nav-bar\":[\"back-button\",\"forward-button\",\"stop-reload-button\",\"new-tab-button\",\"customizableui-special-spring1\",\"urlbar-container\",\"customizableui-special-spring2\",\"downloads-button\",\"unified-extensions-button\",\"ublock0_raymondhill_net-browser-action\"],\"toolbar-menubar\":[\"menubar-items\"],\"TabsToolbar\":[\"tabbrowser-tabs\",\"alltabs-button\"],\"PersonalToolbar\":[\"personal-bookmarks\"]},\"seen\":[\"save-to-pocket-button\",\"developer-button\",\"addon_darkreader_org-browser-action\",\"firefoxpwa_filips_si-browser-action\",\"ublock0_raymondhill_net-browser-action\"],\"dirtyAreaCache\":[\"nav-bar\",\"toolbar-menubar\",\"TabsToolbar\",\"PersonalToolbar\",\"unified-extensions-area\"],\"currentVersion\":20,\"newElementCount\":3}");
user_pref("svg.context-properties.content.enabled", true);
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("widget.gtk.rounded-bottom-corners.enabled", true);

// Hardware decode on Linux has always been half-broken in Firefox, usually
// because it prefers to err on the side of disabling support unless it's very
// sure everything will work. That's sensible in theory, but the end result is
// users with working VA-API setups getting software fallback unless they go
// flipping prefs.
//
// These just force it on. If the stack isn't working, we'll know soon enough,
// and if it is, we get low CPU usage and proper playback, which is kind of the
// whole point of acceleration.
//
user_pref("widget.dmabuf.force-enabled", true);
user_pref("media.ffmpeg.vaapi.enabled", true);
user_pref("media.hardware-video-decoding.force-enabled", true);

// Enables file dialogs via the XDG portal system. This is how modern sandboxed
// apps are supposed to do things, especially under Flatpak or Wayland, where
// direct GTK or Qt dialogs may not even work right.
//
// Without this, we get inconsistent styling or outright failure to access files
// outside the sandbox. With it, things feel like part of the system again.
//
user_pref("widget.use-xdg-desktop-portal.file-picker", 1);
