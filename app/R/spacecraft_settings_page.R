# spacecraft_settings_page.R
# Settings page: the sticky Save bar above two tabs, Variants and Line settings.

sc_settings_ui <- function() {
  nav_panel(
    "Settings",
    div(class = "px-2 pt-2",
        sc_save_bar_ui(),
        navset_card_underline(id = "settings_tabs", sc_settings_variants_tab(), sc_settings_line_tab()))
  )
}
