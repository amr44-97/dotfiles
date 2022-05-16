--require'lspconfig'.zls.setup{}
return {
   languageserver = {
       zls = {
           cmd = {"/home/amr/.local/bin/zls"},
           filetypes =  {"zig"},
       }
   }
}
