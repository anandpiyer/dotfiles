--[[
Hyper key mode. 
https://github.com/lodestone/hyper-hacks/
https://gist.github.com/ttscoff/cce98a711b5476166792d5e6f1ac5907
--]]
-- A global variable for the Hyper mode.
k = hs.hotkey.modal.new({}, "F17")

-- Enter Hyper Mode when F18 (Hyper/Capslock) is pressed
pressedF18 = function()
  k.triggered = false
  k:enter()
end

-- Leave Hyper Mode when F18 (Hyper/Capslock) is pressed,
--   send ESCAPE if no other keys are pressed.
releasedF18 = function()
  k:exit()
  if not k.triggered then
    hs.eventtap.keyStroke({}, 'ESCAPE')
  end
end

-- Bind the Hyper key
f18 = hs.hotkey.bind({}, 'F18', pressedF18, releasedF18)

-------------------------------------------------------------------------------
-- Window Management 
-------------------------------------------------------------------------------
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
    k:bind({}, kv[1], function() kv[2](); k.triggered=true; end)
end
