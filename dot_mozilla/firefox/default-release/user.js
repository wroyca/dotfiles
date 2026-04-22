// Note that ideally we would want to disable auto-refresh (via
// <meta http-equiv="refresh"> or HTTP headers). The problem is that Firefox
// uses the same machinery for external protocol launches. If we block it,
// protocol redirects will prompt with a confirmation dialog every single
// time. So we leave it on, as getting nagged constantly is worse than the
// occasional wiped form.
//
user_pref("accessibility.blockautorefresh", false);

// Prevent keyboard navigation in the URL bar from focusing the action buttons
// on each suggestion row. Tabbing through results should iterate over the
// items linearly, without getting stuck on secondary menus.
//
user_pref("browser.urlbar.resultMenu.keyboardAccessible", false);

// Keep the new tab page clean. By default, it is populated with sponsored
// links and Pocket recommendations. We disable all algorithmic
// and promotional content to maintain a blank slate.
//
user_pref("browser.newtabpage.activity-stream.discoverystream.saveToPocketCard.enabled", false);
user_pref("browser.newtabpage.activity-stream.discoverystream.sendToPocket.enabled", false);
user_pref("browser.newtabpage.activity-stream.feeds.section.topstories", false);
user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);
user_pref("browser.newtabpage.activity-stream.section.highlights.includePocket", false);
user_pref("browser.newtabpage.activity-stream.showSponsored", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("extensions.pocket.enabled", false);

// Disable the browser.ml.* machinery. Recent versions bundle various ML/AI
// features (chat prompts, summaries, etc.) that do not belong on the critical
// path. Note that if future releases scatter these settings behind new flags,
// we will have to track them down again.
//
user_pref("browser.ml.enable", false);

// Integrate with GNOME. The default theme does not quite feel native, so we
// rely on the community GNOME theme to correct the mismatches. We also apply
// a few layout adjustments, such as one-tab hiding and bookmark bar
// placement.
//
user_pref("gnomeTheme.hideSingleTab", true);
user_pref("gnomeTheme.bookmarksToolbarUnderTabs", true);
user_pref("browser.theme.dark-private-windows", false);
user_pref("browser.uiCustomization.state", "{\"placements\":{\"widget-overflow-fixed-list\":[],\"unified-extensions-area\":[\"addon_darkreader_org-browser-action\",\"firefoxpwa_filips_si-browser-action\"],\"nav-bar\":[\"back-button\",\"forward-button\",\"stop-reload-button\",\"new-tab-button\",\"customizableui-special-spring1\",\"urlbar-container\",\"customizableui-special-spring2\",\"downloads-button\",\"unified-extensions-button\",\"ublock0_raymondhill_net-browser-action\"],\"toolbar-menubar\":[\"menubar-items\"],\"TabsToolbar\":[\"tabbrowser-tabs\",\"alltabs-button\"],\"PersonalToolbar\":[\"personal-bookmarks\"]},\"seen\":[\"save-to-pocket-button\",\"developer-button\",\"addon_darkreader_org-browser-action\",\"firefoxpwa_filips_si-browser-action\",\"ublock0_raymondhill_net-browser-action\"],\"dirtyAreaCache\":[\"nav-bar\",\"toolbar-menubar\",\"TabsToolbar\",\"PersonalToolbar\",\"unified-extensions-area\"],\"currentVersion\":20,\"newElementCount\":3}");
user_pref("svg.context-properties.content.enabled", true);
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("widget.gtk.rounded-bottom-corners.enabled", true);

// Force hardware video decoding (VA-API/dmabuf). Firefox
// disables hardware decode on Linux unless it is absolutely sure the driver
// stack works, which usually results in an unnecessary software fallback. We
// force it on. If the stack is broken, it will fail visibly, but otherwise
// we get actual hardware acceleration.
//
user_pref("widget.dmabuf.force-enabled", true);
user_pref("media.ffmpeg.vaapi.enabled", true);
user_pref("media.hardware-video-decoding.force-enabled", true);

// Use XDG desktop portals for file dialogs.
//
user_pref("widget.use-xdg-desktop-portal.file-picker", 1);

// Disable tab hover previews.
//
user_pref("browser.tabs.hoverPreview.enabled", false);
user_pref("browser.tabs.hoverPreview.showThumbnails", false);

// Don't trim https://
//
user_pref("browser.urlbar.trimURLs", false);
