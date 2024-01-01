return {
  {
    "norcalli/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup()
    end,
    opts = {
      rgb_fn = true,
      RRGGBBAA = true,
      css = true,
      css_fn = true,
      hsl_fn = true,
    },
  },
}
