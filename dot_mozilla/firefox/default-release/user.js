// Web pages (and web servers) can ask a browser to automatically refresh a
// page after a given timeout by including the HTML element <meta
// http-equiv="refresh"> or by sending a Refresh: HTTP header. This can be
// helpful in the case of a webpage whose content is updated constantly, but it
// can also be irritating and, at worst, abused.
//
user_pref("accessibility.blockautorefresh", true);

// Approximately one year ago, Mozilla added a button to certain suggestions in
// the URL bar. This change breaks the ability to tab through results, as the
// focus is placed on the button instead of the item.
//
user_pref("browser.urlbar.resultMenu.keyboardAccessible", false);

// I already have a trash can under my desk.
//
user_pref("browser.newtabpage.activity-stream.discoverystream.saveToPocketCard.enabled", false);
user_pref("browser.newtabpage.activity-stream.discoverystream.sendToPocket.enabled", false);
user_pref("browser.newtabpage.activity-stream.feeds.section.topstories", false);
user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);
user_pref("browser.newtabpage.activity-stream.section.highlights.includePocket", false);
user_pref("browser.newtabpage.activity-stream.showSponsored", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("extensions.pocket.enabled", false);

// Make Firefox look more at home on GNOME desktop. By default this remodels
// Mozilla’s browser into something that closely resembles GNOME Web.
//
// https://github.com/rafaelmardojai/firefox-gnome-theme
//
user_pref("gnomeTheme.hideSingleTab", true);
user_pref("gnomeTheme.bookmarksToolbarUnderTabs", true);
user_pref("browser.theme.dark-private-windows", false);
user_pref("browser.uiCustomization.state", "{\"placements\":{\"widget-overflow-fixed-list\":[],\"unified-extensions-area\":[\"addon_darkreader_org-browser-action\",\"firefoxpwa_filips_si-browser-action\"],\"nav-bar\":[\"back-button\",\"forward-button\",\"stop-reload-button\",\"new-tab-button\",\"customizableui-special-spring1\",\"urlbar-container\",\"customizableui-special-spring2\",\"downloads-button\",\"unified-extensions-button\",\"ublock0_raymondhill_net-browser-action\"],\"toolbar-menubar\":[\"menubar-items\"],\"TabsToolbar\":[\"tabbrowser-tabs\",\"alltabs-button\"],\"PersonalToolbar\":[\"personal-bookmarks\"]},\"seen\":[\"save-to-pocket-button\",\"developer-button\",\"addon_darkreader_org-browser-action\",\"firefoxpwa_filips_si-browser-action\",\"ublock0_raymondhill_net-browser-action\"],\"dirtyAreaCache\":[\"nav-bar\",\"toolbar-menubar\",\"TabsToolbar\",\"PersonalToolbar\",\"unified-extensions-area\"],\"currentVersion\":20,\"newElementCount\":3}");
user_pref("svg.context-properties.content.enabled", true);
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("widget.gtk.rounded-bottom-corners.enabled", true);

// Force (enable) hardware video acceleration (VA-API) when available.
//
user_pref("widget.dmabuf.force-enabled", true);
user_pref("media.ffmpeg.vaapi.enabled", true);
user_pref("media.hardware-video-decoding.force-enabled", true);

// Force Firefox to always use xdg-desktop-portal.
//
user_pref("widget.use-xdg-desktop-portal.file-picker", 1);

