return {
  [[ggandor/leap-spooky.nvim]], name = [[leap-spooky]],

  config = true,
  keys = function()
		local out = {}
		for _, key1 in ipairs({ [[i]], [[a]] }) do
			for _, key2 in ipairs({ [[r]], [[R]], [[m]], [[M]] }) do
				table.insert(out, { key1 .. key2, mode = [[o]] })
			end
		end
		return out
	end
}
