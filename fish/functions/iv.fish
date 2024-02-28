function iv --wraps='nvim --listen /tmp/nvimsocket' --description 'alias iv nvim --listen /tmp/nvimsocket'
  nvim --listen /tmp/nvim.pipe $argv
        
end
