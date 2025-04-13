// Web pages request automatic refreshes through <meta http-equiv="refresh">
// tags or Refresh: HTTP headers. While useful for auto-updating content,
// this can be disruptive to reading and potentially abused for redirects.
//
user_pref("accessibility.blockautorefresh", true);

// Firefox 89+ URL bar buttons break keyboard navigation - tab key focuses
// buttons instead of cycling through search results.
//
user_pref("browser.urlbar.resultMenu.keyboardAccessible", false);

// Remove Pocket integration, sponsored stories, top sites recommendations,
// and promotional content from the new tab page.
//
user_pref("browser.newtabpage.activity-stream.discoverystream.saveToPocketCard.enabled", false);
user_pref("browser.newtabpage.activity-stream.discoverystream.sendToPocket.enabled", false);
user_pref("browser.newtabpage.activity-stream.feeds.section.topstories", false);
user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);
user_pref("browser.newtabpage.activity-stream.section.highlights.includePocket", false);
user_pref("browser.newtabpage.activity-stream.showSponsored", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("extensions.pocket.enabled", false);

// Apply GNOME theme styling with rounded corners, GTK colors, and native UI elements.
//
// Source: https://github.com/rafaelmardojai/firefox-gnome-theme
//
user_pref("gnomeTheme.hideSingleTab", true);
user_pref("gnomeTheme.bookmarksToolbarUnderTabs", true);
user_pref("browser.theme.dark-private-windows", false);
user_pref("browser.uiCustomization.state", "{\"placements\":{\"widget-overflow-fixed-list\":[],\"unified-extensions-area\":[\"addon_darkreader_org-browser-action\",\"firefoxpwa_filips_si-browser-action\"],\"nav-bar\":[\"back-button\",\"forward-button\",\"stop-reload-button\",\"new-tab-button\",\"customizableui-special-spring1\",\"urlbar-container\",\"customizableui-special-spring2\",\"downloads-button\",\"unified-extensions-button\",\"ublock0_raymondhill_net-browser-action\"],\"toolbar-menubar\":[\"menubar-items\"],\"TabsToolbar\":[\"tabbrowser-tabs\",\"alltabs-button\"],\"PersonalToolbar\":[\"personal-bookmarks\"]},\"seen\":[\"save-to-pocket-button\",\"developer-button\",\"addon_darkreader_org-browser-action\",\"firefoxpwa_filips_si-browser-action\",\"ublock0_raymondhill_net-browser-action\"],\"dirtyAreaCache\":[\"nav-bar\",\"toolbar-menubar\",\"TabsToolbar\",\"PersonalToolbar\",\"unified-extensions-area\"],\"currentVersion\":20,\"newElementCount\":3}");
user_pref("svg.context-properties.content.enabled", true);
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("widget.gtk.rounded-bottom-corners.enabled", true);

// Forces GPU video acceleration regardless of Firefox's hardware support
// detection. VA-API must be properly configured in the system.
//
user_pref("widget.dmabuf.force-enabled", true);
user_pref("media.ffmpeg.vaapi.enabled", true);
user_pref("media.hardware-video-decoding.force-enabled", true);

// Use XDG desktop portal for file dialogs with native appearance and
// system-wide access controls.
//
user_pref("widget.use-xdg-desktop-portal.file-picker", 1);
