local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = ";"

require("lazy").setup({

	{ "folke/neoconf.nvim",      cmd = "Neoconf" },
	"folke/neodev.nvim",
	{
		'ishan9299/modus-theme-vim',
		lazy = false,
		priority = 1000,
		config = function()
			-- load the colorscheme here
			vim.cmd([[colorscheme modus-vivendi]])
		end,
	},

	'ziglang/zig.vim',

	{
		'nvim-telescope/telescope.nvim',
		dependencies = { 'nvim-lua/plenary.nvim' }
	},

	{
		"windwp/nvim-autopairs",
		config = function()
			local status_ok, npairs = pcall(require, "nvim-autopairs")
			if not status_ok then
				return
			end


			npairs.setup {
				check_ts = true,
				ts_config = {
					lua = { "string", "source" },
					javascript = { "string", "template_string" },
				},
				disable_filetype = { "TelescopePrompt" },
				fast_wrap = {
					map = '<M-e>',
					chars = { '{', '[', '(', '"', "'" },
					pattern = [=[[%'%"%)%>%]%)%}%,]]=],
					end_key = '$',
					keys = 'qwertyuiopzxcvbnmasdfghjkl',
					check_comma = true,
					highlight = 'Search',
					highlight_grey = 'Comment'
				},

			}
			local cmp_autopairs = require "nvim-autopairs.completion.cmp"
			local cmp_stat_ook, cmp = pcall(require, "cmp")
			if not cmp_stat_ook then
				return
			end
			cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done { map_char = { tex = "" } })
		end,
	},

	{
		"numToStr/Comment.nvim", -- "gc" to comment visual regions/lines
		config = function()
			require('Comment').setup()
		end,

	},


	'neovim/nvim-lspconfig',
	'hrsh7th/cmp-nvim-lsp',
	'hrsh7th/cmp-buffer',
	'hrsh7th/cmp-path',
	'hrsh7th/cmp-cmdline',
	'hrsh7th/nvim-cmp',
	'hrsh7th/cmp-vsnip',
	'hrsh7th/vim-vsnip',
	'hrsh7th/vim-vsnip-integ',
	--'L3MON4D3/LuaSnip',
	-- 'saadparwaiz1/cmp_luasnip',

	{ 'akinsho/toggleterm.nvim', version = "*",  config = true },

	{
		"X3eRo0/dired.nvim",
		dependencies = "MunifTanjim/nui.nvim",
		config = function()
			vim.g.loaded_netrw = 1
			vim.g.loaded_netrwPlugin = 1
			require("dired").setup {
				path_separator = "/",
				show_banner = false,
				show_hidden = true,
				show_dot_dirs = true,
				show_colors = true,
			}
		end
	},


	{
		'nvim-lualine/lualine.nvim',
		config = function()
			require('lualine').setup {
				options = {
					icons_enabled = true,
					theme = 'auto',
					component_separators = '|',
					section_separators = '',

				},
			}
		end,
	},

	{
		'nvim-tree/nvim-web-devicons',
		config = function()
			require('nvim-web-devicons').setup {}
		end,
	},


})
