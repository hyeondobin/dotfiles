function vim --wraps='nvim' --description 'alias vim nvim'
  nvim --listen /tmp/godothost $argv
end
