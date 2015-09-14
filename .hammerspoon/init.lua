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
local first       = hs.screen.allScreens()[3]
local second      = hs.screen.allScreens()[2]
local third       = hs.screen.allScreens()[1]

function sendTo(area)
  local win    = hs.window.focusedWindow()
  local screen = win:screen()
  hs.grid.set(win,area,screen)
end

function selectWindow()
  hs.hints.windowHints()
end

function moveToScreen(screen)
  local win = hs.window.focusedWindow()
  win:moveToScreen(screen,0)
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

hs.hotkey.bind({"cmd","ctrl"}, 'f', function() selectWindow() end)
k:bind({"shift"}, 'a', function() moveToScreen(first) end)
k:bind({"shift"}, 'o', function() moveToScreen(second) end)
k:bind({"shift"}, 'e', function() moveToScreen(third) end)

function reload_config(files)
  hs.reload()
end
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reload_config):start()
hs.alert.show("Config loaded")
