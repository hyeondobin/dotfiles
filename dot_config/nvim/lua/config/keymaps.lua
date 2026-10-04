local function map(mode)
    return function(lhs, rhs, desc, opts)
        opts = opts or {}
        if type(desc) == "string" then
            opts.desc = desc
        elseif type(desc) == "table" then
            opts = desc
        end
        vim.keymap.set(mode, lhs, rhs, opts)
    end
end

local nmap = map("n")
local imap = map("i")
local vmap = map("x")
local tmap = map("t")

nmap("<Space>", "<nop>")
nmap("<Esc>", "<nop>")

nmap("<M-w>", vim.cmd.xa, { desc = "Save and Quit" })
nmap("<leader>w", vim.cmd.w, { desc = "Save current file" })
nmap("<leader>y", [["+y]], "+에 복사")
vmap("<leader>y", [["+y]], "+에 복사")
nmap("<leader>p", [["+p]], "+에 복사")

nmap("j", "gj")
nmap("k", "gk")

nmap("<C-h>", "<C-w>h")
nmap("<C-l>", "<C-w>l")
nmap("<C-j>", "<C-w>j")
nmap("<C-k>", "<C-w>k")

nmap("H", ":bp<CR>")
nmap("L", ":bn<CR>")

imap("<C-c>", "<Esc>")

vmap("<A-j>", ":<C-u>execute \"'<,'>move '>+\" . v:count1<cr>gv=gv", { desc = "Move Selected Down" })
vmap("<A-k>", ":<C-u>execute \"'<,'>move '<-\" . (v:count1 + 1)<cr>gv=gv", { desc = "Move Selected Up" })

nmap("<A-j>", "<cmd>execute 'move .+' . v:count1<cr>==", { desc = "Move Current Line Down" })
nmap("<A-k>", "<cmd>execute 'move .-' . (v:count1 +1)<cr>==", { desc = "Move Current Line Up" })

imap("<A-j>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move Current Line Down" })
imap("<A-k>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move Current Line Up" })

-- center buffer and open folds when navigating
nmap("<C-u>", "<C-u>zz")
nmap("<C-d>", "<C-d>zz")
nmap("<C-i>", "<C-i>zz")
nmap("<C-o>", "<C-o>zz")
nmap("n", "nzzzv")
nmap("N", "Nzzzv")
nmap("%", "%zzzv")
nmap("*", "*zzzv")
nmap("#", "#zzzv")
nmap("{", "{zz")
nmap("}", "}zz")

-- easy rename
nmap("S", [[:%s/\<<C-r><C-w>\>//gI<Left><Left><Left>]], { desc = "현재 단어 일괄 치환" })
vmap("S", [[:s//gI<Left><Left><Left>]], { desc = "선택한 단어 일괄 치환" })

-- keep in/outdenting
vmap("<", "<gv")
vmap(">", ">gv")

-- nvim as a calculator
imap("<C-=>", function()
    local line = vim.api.nvim_get_current_line()
    local expr = line:match("([^=]+)$") or line
    local fn = load("return " .. expr)
    if fn then
        local ok, res = pcall(fn)
        if ok and res ~= nil then
            vim.api.nvim_set_current_line(line .. " = " .. tostring(res))
            vim.api.nvim_win_set_cursor(0, { vim.api.nvim_win_get_cursor(0)[1], #vim.api.nvim_get_current_line() })
        end
    end
end, { desc = "현재 줄 수식 계산" })

nmap("<leader>fws", "<cmd>w|so<cr>", { desc = "Save and source current file" })

tmap("<esc><esc>", [[<C-\><C-n>]], "Enter normal mode with double Esc")
tmap("<C-h>", [[<C-\><C-n><C-w>h]], "Move to left window")
tmap("<C-j>", [[<C-\><C-n><C-w>j]], "Move to bottom window")
tmap("<C-k>", [[<C-\><C-n><C-w>k]], "Move to top window")
tmap("<C-l>", [[<C-\><C-n><C-w>l]], "Move to right window")

imap("<A-;>", "<esc>A;<cr>", "줄 끝에 ';' 추가 후 개행")
imap("<A-,>", "<esc>A,", "줄 끝에 ',' 추가")

vmap("<leader>s", ":sort<CR>", "선택 영역 정렬")

-- jujutsu and chezmoi
nmap("<leader>jc", "<cmd>!chezmoi re-add && jj -R $(chezmoi source-path) new<CR>", "jj new (Chezmoi re-add)")
nmap("<leader>jn", "<cmd>!jj -R $(chezmoi source-path) new<CR>", "jj new")
