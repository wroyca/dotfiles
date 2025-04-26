.PHONY: test doc clean all

all: test doc

test:
	@echo "Running tests with mini.test..."
	@nvim --headless \
		-c "lua vim.opt.runtimepath:append('.')" \
		-c "lua vim.opt.runtimepath:append('./pack/dist/opt/mini.test')" \
		-c "lua require('mini.test').run()" \
		-c "quitall"

doc:
	@echo "Generating documentation with mini.doc..."
	@nvim --headless \
		-c "lua vim.opt.runtimepath:append('.')" \
		-c "lua vim.opt.runtimepath:append('./pack/dist/opt/mini.doc')" \
		-c "lua require('mini.doc').generate()" \
		-c "quitall"

clean:
	@echo "Cleaning generated files..."
	@rm -f doc/*.txt
	@echo "Done!"

help:
	@echo "Available targets:"
	@echo "  all    - Run test and doc (default)"
	@echo "  test   - Run tests using mini.test"
	@echo "  doc    - Generate documentation using mini.doc"
	@echo "  clean  - Clean generated files"
	@echo "  help   - Show this help message"
