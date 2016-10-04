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

keysWindowFunctions = {
    {'f', positionWindow(0,0,1,1)},     -- Hyper+F Full screen
    {'h', positionWindow(0,0,0.5,1)},   -- Hyper+H Left 50%
    {'l', positionWindow(0.5,0,0.5,1)}, -- Hyper+L Right 50%
    {'j', positionWindow(0,0.5,1,0.5)}, -- Hyper+J Bottom 50%
    {'k', positionWindow(0,0,1,0.5)},    -- Hyper+K Top 50%
    {'left', moveWindow(-10,0,0,0)},
    {'right', moveWindow(10,0)},
    {'up', moveWindow(0,-10)},
    {'down', moveWindow(0,10)}
}

for iter,kv in ipairs(keysWindowFunctions) do
    k:bind({}, kv[1], function() kv[2](); k.triggered=true; end)
end

