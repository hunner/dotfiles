hs.grid.GRIDHEIGHT = 2
hs.grid.GRIDWIDTH = 2
hs.grid.MARGINX = 0
hs.grid.MARGINY = 0

k = hs.hotkey.modal.new({"cmd","shift"}, "space")

function k:entered() hs.alert.show('↩') end
function k:exited()  hs.alert.show('⎋') end

local top         = {x=0,y=0,w=2,h=1}
local bottom      = {x=0,y=1,w=2,h=1}
local left        = {x=0,y=0,w=1,h=2}
local right       = {x=1,y=0,w=1,h=2}
local topleft     = {x=0,y=0,w=1,h=1}
local topright    = {x=1,y=0,w=1,h=1}
local bottomleft  = {x=0,y=1,w=1,h=1}
local bottomright = {x=1,y=1,w=1,h=1}
local full        = {x=0,y=0,w=2,h=2}

function sendTo(area)
  local win    = hs.window.focusedWindow()
  local screen = hs.screen.mainScreen()
  hs.grid.set(win,area,screen)
end

k:bind({}, 'escape', function() k:exit() end)
k:bind({}, 'h', function() sendTo(left) end)
k:bind({}, 'j', function() sendTo(bottom) end)
k:bind({}, 'k', function() sendTo(top) end)
k:bind({}, 'l', function() sendTo(right) end)
k:bind({}, 'y', function() sendTo(topleft) end)
k:bind({}, 'u', function() sendTo(topright) end)
k:bind({}, 'b', function() sendTo(bottomleft) end)
k:bind({}, 'n', function() sendTo(bottomright) end)
k:bind({}, 'f', function() sendTo(full) end)

function reload_config(files)
  hs.reload()
end
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reload_config):start()
hs.alert.show("Config loaded")
