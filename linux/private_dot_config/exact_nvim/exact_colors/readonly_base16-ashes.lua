local use_cterm, palette

--base00 - Default Background
--base01 - Lighter Background (Used for status bars, line number and folding marks)
--base02 - Selection Background
--base03 - Comments, Invisibles, Line Highlighting
--base04 - Dark Foreground (Used for status bars)
--base05 - Default Foreground, Caret, Delimiters, Operators
--base06 - Light Foreground (Not often used)
--base07 - Light Background (Not often used)
--base08 - Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
--base09 - Integers, Boolean, Constants, XML Attributes, Markup Link Url
--base0A - Classes, Markup Bold, Search Text Background
--base0B - Strings, Inherited Class, Markup Code, Diff Inserted
--base0C - Support, Regular Expressions, Escape Characters, Markup Quotes
--base0D - Functions, Methods, Attribute IDs, Headings
--base0E - Keywords, Storage, Selector, Markup Italic, Diff Changed
--base0F - Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>

palette = {
	base00 = "#1C2023",
	base01 = "#393F45",
	base02 = "#565E65",
	base03 = "#747C84",
	base04 = "#ADB3BA",
	base05 = "#C7CCD1",
	base06 = "#DFE2E5",
	base07 = "#F3F4F5",
	base08 = "#C7AE95",
	base09 = "#C7C795",
	base0A = "#AEC795",
	base0B = "#95C7AE",
	base0C = "#95AEC7",
	base0D = "#AE95C7",
	base0E = "#C795AE",
	base0F = "#C79595",
}

if palette then
  require('mini.base16').setup({ palette = palette, use_cterm = use_cterm })
  vim.g.colors_name = "base16-ashes"
end
