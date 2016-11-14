local hotkey = require "hs.hotkey"
local alert = require "hs.alert"
local caffeinate = require "hs.caffeinate"

hyper = {"cmd", "alt", "ctrl", "shift"}

-- -----------------------------------------------------------------------------
-- System Management
-- -----------------------------------------------------------------------------

hs.hotkey.bind({"cmd", "shift"}, "l", function()
  caffeinate.lockScreen()
end)

local caffeine = hs.menubar.new()
local activeMessage = "Sleeping prohitited"
local inactiveMessage = "Sleeping allowed"
function setCaffeineDisplay(state)
  if state then
    caffeine:setIcon("caffeine-active.png")
    caffeine:setTooltip(activeMessage)
    hs.alert.show(activeMessage)
  else
    caffeine:setIcon("caffeine-inactive.png")
    caffeine:setTooltip(inactiveMessage)
    hs.alert.show(inactiveMessage)
  end
end

function caffeineClicked()
  setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end

if caffeine then
  caffeine:setClickCallback(caffeineClicked)
  setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
end

hs.hotkey.bind({"cmd","shift"},"c", function()
      setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end)

-- -----------------------------------------------------------------------------
-- Window Management 
-- -----------------------------------------------------------------------------
hs.window.animationDuration = 0

local window = require "hs.window"
local grid = require "hs.grid"

grid.MARGINX = 0
grid.MARGINY = 0
local gw = grid.GRIDWIDTH
local gh = grid.GRIDHEIGHT

local setCellForWindow = function(cell)
    return function()
        local win = window.focusedWindow()
        if win then
            grid.set(win, cell)
        else
            alert.show("Please select a window!")
        end
    end 
end

positionWindow = function(x, y, w, h)
    return function()
        local win = hs.window.focusedWindow()
        local screen = win:screen()
        local f = win:frame()
        local max = screen:frame()
    
        f.x = max.w * x
        f.y = max.h * y
        f.w = max.w * w
        f.h = max.h * h
        win:setFrame(f)
    end
end

moveWindow = function(x, y)
    return function()
        local win = hs.window.focusedWindow()
        local f = win:frame()
        f.x = f.x + x
        f.y = f.y + y
        win:setFrame(f)
    end
end

local leftHalf = {x=0,y=0,w=gw/2,h=gh}
local rightHalf = {x=gw/2,y=0,w=gw/2,h=gh}
local topHalf = {x=0,y=0,w=gw,h=gw/2}
local bottomHalf = {x=0,y=gh/2,w=gw,h=gw/2}
local topLeft = {x=0,y=0,w=gw/2,h=gh/2}
local topRight = {x=gw/2,y=0,w=gw/2,h=gh/2}
local bottomLeft = {x=0,y=gh/2,w=gw/2,h=gh/2}
local bottomRight = {x=gw/2,y=gh/2,w=gw/2,h=gh/2}

keysWindowFunctions = {
    {'f', grid.maximizeWindow},
    {'h', setCellForWindow(leftHalf)},
    {'l', setCellForWindow(rightHalf)},
    {'j', setCellForWindow(bottomHalf)},
    {'k', setCellForWindow(topHalf)},
    {'left', moveWindow(-10,0)},        -- Hyper+left Move window left
    {'right', moveWindow(10,0)},        -- Hyper+right Move window right
    {'up', moveWindow(0,-10)},          -- Hyper+up Move window up
    {'down', moveWindow(0,10)},         -- Hyper+down Move window down
    {'1', setCellForWindow(topLeft)},
    {'2', setCellForWindow(topRight)},
    {'3', setCellForWindow(bottomLeft)},
    {'4', setCellForWindow(bottomRight)}
}

for i,kv in ipairs(keysWindowFunctions) do
--    k:bind({}, kv[1], function() kv[2](); k.triggered=true; end)
   hs.hotkey.bind(hyper, kv[1], function() kv[2](); end) 
end

switcher = hs.window.switcher.new() 
hs.hotkey.bind(hyper,'tab',nil,function()switcher:next()end,nil,function()switcher:next()end)

-- -----------------------------------------------------------------------------
-- Application Management
-- -----------------------------------------------------------------------------
local application = require "hs.application"

a = hs.hotkey.modal.new({}, "F16")

launch = function(appName)
    local app = application.get(appName) 
    if not app then 
        alert.show(appName .. " is not active!") 
        return 
    end
    application.launchOrFocus(app:name()) 
    app:activate(true) 
end

keysApps = {
    {'i', "iTerm2"},
    {'s', "Safari"},
    {'a', "Airmail"},
    {'f', "Finder"},
    {'e', "Emacs"}
}

for i, app in ipairs(keysApps) do
  a:bind({}, app[1], function() launch(app[2]); a:exit(); end)
end

pressedA = function() a:enter() end
releasedA = function() end
hs.hotkey.bind(hyper, 'a', nil, pressedA, releasedA)

-- -----------------------------------------------------------------------------
-- Reload config automatically upon change.
-- -----------------------------------------------------------------------------
function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
local myWatcher = hs.pathwatcher.new("/Users/api/dotfiles/hammerspoon/", reloadConfig):start()
hs.alert.show("Hammerspoon config loaded")

