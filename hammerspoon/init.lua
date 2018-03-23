local hotkey = require "hs.hotkey"
local alert = require "hs.alert"

hyper = {"cmd", "alt", "ctrl", "shift"}

-- -----------------------------------------------------------------------------
-- System Management
-- -----------------------------------------------------------------------------
local caffeinate = require "hs.caffeinate"

-- Lockscreen
hotkey.bind({"cmd", "shift"}, "l", function()
  caffeinate.lockScreen()
end)

-- Caffeine functionality. Icon images from KeepingYouAwake.
local caffeine = hs.menubar.new()
local activeMessage = "Sleeping prohitited"
local inactiveMessage = "Sleeping allowed"
function setCaffeineDisplay(state)
  if state then
    caffeine:setIcon("caffeine-active.png")
    caffeine:setTooltip(activeMessage)
    alert.show(activeMessage)
  else
    caffeine:setIcon("caffeine-inactive.png")
    caffeine:setTooltip(inactiveMessage)
    alert.show(inactiveMessage)
  end
end

function caffeineClicked()
  setCaffeineDisplay(caffeinate.toggle("displayIdle"))
end

if caffeine then
  caffeine:setClickCallback(caffeineClicked)
  setCaffeineDisplay(caffeinate.get("displayIdle"))
end

hotkey.bind({"cmd","shift"},"c", function()
      setCaffeineDisplay(caffeinate.toggle("displayIdle"))
end)

-- -----------------------------------------------------------------------------
-- Window Management 
-- -----------------------------------------------------------------------------
hs.loadSpoon("MiroWindowsManager")
hs.window.animationDuration = 0.1
spoon.MiroWindowsManager:bindHotkeys({
  up = {hyper, "up"},
  right = {hyper, "right"},
  down = {hyper, "down"},
  left = {hyper, "left"},
  fullscreen = {hyper, "F"}
})

-- -----------------------------------------------------------------------------
-- Window Management with ChunkWM
-- -----------------------------------------------------------------------------
keysWindowFunctions = {
    {'0', "chunkc tiling::desktop --equalize"}, -- equalize size of windows.
    {'f', "chunkc tiling::window --toggle fullscreen"},
    {'s', "chunkc tiling::window --swap prev"}
}

for i,kv in ipairs(keysWindowFunctions) do
   hs.hotkey.bind(hyper, kv[1], function() hs.execute(kv[2], true); end) 
end

-- -----------------------------------------------------------------------------
-- Application Management
-- -----------------------------------------------------------------------------
local application = require "hs.application"

myModal = hotkey.modal.new({}, "F16")

keysApps = {
    {key = 'b', name = 'Firefox'},
    {key = 'e', name = 'Emacs'},
    {key = 'f', name = 'Finder'},
    {key = 'i', name = 'iTerm'},
    {key = 's', name = 'Safari'},
    {key = 't', name = 'Terminal'},
}

for _, app in ipairs(keysApps) do
  if app.id then
    local located_name = hs.application.nameForBundleID(app.id)
    if located_name then
      myModal:bind('', app.key, located_name, function()
          hs.application.launchOrFocusByBundleID(app.id)
          myModal:exit()
      end)
    end
  elseif app.name then
      myModal:bind('', app.key, app.name, function()
          hs.application.launchOrFocus(app.name)
          myModal:exit()
      end)
  end
end

pressedA = function() myModal:enter() end
releasedA = function() end
hotkey.bind(hyper, 'a', nil, pressedA, releasedA)

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
alert.show("Hammerspoon config loaded")

