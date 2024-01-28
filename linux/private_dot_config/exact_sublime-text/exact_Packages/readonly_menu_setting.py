import sublime
import sublime_plugin

class MenuSetting(sublime_plugin.EventListener):
  def on_activated(self, view):
    show_menu = view.settings().get('show_menu')
    if show_menu:
      view.window().set_menu_visible(True)
    elif show_menu is not None:
      view.window().set_menu_visible(False)
