local status_ok, _ = pcall(require, "lspconfig")
if not status_ok then
  return
end


require  "lsp.settings.zls"
require  "lsp.settings.sumneko_lua"


require "lsp.configs"
require("lsp.handlers").setup()
require "lsp.null-ls"
