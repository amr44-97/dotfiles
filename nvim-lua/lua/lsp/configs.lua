local status_ok, lsp_installer = pcall(require, "nvim-lsp-installer")
if not status_ok then
	return
end

local lspconfig = require("lspconfig")

local servers = { "zig_set","sumneko" }

lsp_installer.setup {
	ensure_installed = servers
}

