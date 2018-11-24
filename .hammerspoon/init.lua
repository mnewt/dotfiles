--------------------------------------------------------------------------------
-- * Global stuff
--------------------------------------------------------------------------------

local mash = {"cmd", "alt", "ctrl"}
local mashshift = {"cmd", "alt", "ctrl", "shift"}

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

-- ** Convert a table to a string
function table.val_to_str ( v )
   if "string" == type( v ) then
      v = string.gsub( v, "\n", "\\n" )
      if string.match( string.gsub(v,"[^'\"]",""), '^"+$' ) then
         return "'" .. v .. "'"
      end
      return '"' .. string.gsub(v,'"', '\\"' ) .. '"'
   else
      return "table" == type( v ) and table.tostring( v ) or
         tostring( v )
   end
end

function table.key_to_str ( k )
   if "string" == type( k ) and string.match( k, "^[_%a][_%a%d]*$" ) then
      return k
   else
      return "[" .. table.val_to_str( k ) .. "]"
   end
end

function table.tostring( tbl )
   local result, done = {}, {}
   for k, v in ipairs( tbl ) do
      table.insert( result, table.val_to_str( v ) )
      done[ k ] = true
   end
   for k, v in pairs( tbl ) do
      if not done[ k ] then
         table.insert( result,
                       table.key_to_str( k ) .. "=" .. table.val_to_str( v ) )
      end
   end
   return "{" .. table.concat( result, "," ) .. "}"
end

-- ** Get output of a bash command
function os.capture(cmd)
   local f = assert(io.popen(cmd, 'r'))
   local s = assert(f:read('*a'))
   f:close()
   s = string.gsub(s, '^%s+', '')
   s = string.gsub(s, '%s+$', '')
   s = string.gsub(s, '[\n\r]+', ' ')
   return s
end

-- ** Execute and print to console
function executeAndPrint(cmd)
   print("Executing command:")
   print(cmd)
   out, err = hs.execute(cmd)
   print(out)
   if (err ~= nil) then
      print("Command errored.")
      print(string.format("Command exited with status: %d", err))
   end
end

-- ** Check if file exists
function file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end

-- ** Load file if it exists
function dofile_if(name)
   if (file_exists(name)) then
      print("Loading " .. name .. "...")
      dofile(name)
   end
end

-- ** Defeat paste blocking
hs.hotkey.bind(mash, "v", function() hs.eventtap.keyStrokes(hs.pasteboard.getContents()) end)

--------------------------------------------------------------------------------
-- * Hammerspoon config utlities
--------------------------------------------------------------------------------

-- ** Automatically reload Hammerspoon config

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
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Config loaded")

hs.hotkey.bind(mash, 'r', hs.reload)

-- ** Open Hammerspoon console
hs.hotkey.bind(mash, "'", hs.openConsole)

-- ** Open Hammerspoon preferences
hs.hotkey.bind(mash, ",", hs.openPreferences)

-- ** Dark mode
local console = require("hs.console")
console.smartInsertDeleteEnabled(false)
if console.darkMode() then
   console.outputBackgroundColor{ white = 0 }
   console.consoleCommandColor{ white = 1 }
   console.alpha(.8)
else
   console.windowBackgroundColor({red=.6,blue=.7,green=.7})
   console.outputBackgroundColor({red=.8,blue=.8,green=.8})
   console.alpha(.9)
end

--------------------------------------------------------------------------------
-- * Window tiling
--------------------------------------------------------------------------------

-- ** Undo
local undo = require 'undo'
hs.hotkey.bind(mash, "z", function() undo:undo() end)

-- disable animations
hs.window.animationDuration = 0

hs.hotkey.bind(mash, "F", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w
      f.h = max.h
      undo:addToStack()
      win:setFrame(f)
end)

hs.hotkey.bind(mash, "Left", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h
      undo:addToStack()
      win:setFrame(f)
end)

hs.hotkey.bind(mash, "Right", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x + (max.w / 2)
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h
      undo:addToStack()
      win:setFrame(f)
end)

hs.hotkey.bind(mash, "Up", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w
      f.h = max.h / 2
      undo:addToStack()
      win:setFrame(f)
end)

hs.hotkey.bind(mash, "Down", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y + (max.h / 2)
      f.w = max.w
      f.h = max.h / 2
      undo:addToStack()
      win:setFrame(f)
end)

--------------------------------------------------------------------------------
-- * Window moving and resizing
--------------------------------------------------------------------------------
-- Credit to GitHub user: ztomer

hs.grid.MARGINX = 0
hs.grid.MARGINY = 0
hs.grid.GRIDHEIGHT = 18
hs.grid.GRIDWIDTH = 18

-- ** Alter gridsize
hs.hotkey.bind(mashshift, '=', function()
                  undo:addToStack() hs.grid.adjustHeight(1)
end)
hs.hotkey.bind(mashshift, '-', function()
                  undo:addToStack()
                  hs.grid.adjustHeight(-1)
end)
hs.hotkey.bind(mash, '=', function()
                  undo:addToStack()
                  hs.grid.adjustWidth(1)
end)
hs.hotkey.bind(mash, '-', function()
                  undo:addToStack()
                  hs.grid.adjustWidth(-1)
end)

-- ** Snap windows
hs.hotkey.bind(mash, ';', function()
                  undo:addToStack()
                  hs.grid.snap(hs.window.focusedWindow())
end)
hs.hotkey.bind(mash, "'", function()
                  undo:addToStack()
                  hs.fnutils.map(hs.window.visibleWindows(), hs.grid.snap)
end)

-- ** Move windows
hs.hotkey.bind(mash, 'j', function()
                  undo:addToStack()
                  hs.grid.pushWindowDown()
end)
hs.hotkey.bind(mash, 'k', function()
                  undo:addToStack()
                  hs.grid.pushWindowUp()
end)
hs.hotkey.bind(mash, 'h', function()
                  undo:addToStack()
                  hs.grid.pushWindowLeft()
end)
hs.hotkey.bind(mash, 'l', function()
                  undo:addToStack()
                  hs.grid.pushWindowRight()
end)

--** Resize windows
hs.hotkey.bind(mashshift, 'k', function()
                  undo:addToStack()
                  hs.grid.resizeWindowShorter()
end)
hs.hotkey.bind(mashshift, 'j', function()
                  undo:addToStack()
                  hs.grid.resizeWindowTaller()
end)
hs.hotkey.bind(mashshift, 'l', function()
                  undo:addToStack()
                  hs.grid.resizeWindowWider()
end)
hs.hotkey.bind(mashshift, 'h', function()
                  undo:addToStack()
                  hs.grid.resizeWindowThinner()
end)

-- ** Toggle window zoom (acts like Alt+Shift+GreenPlusButton)
hs.hotkey.bind(mash, "m", function()
                  local win = hs.window.focusedWindow()
                  local frame = win:frame()
                  local id = win:id()

                  -- init table to save window state
                  savedwin = savedwin or {}
                  savedwin[id] = savedwin[id] or {}

                  undo:addToStack()
                  if (savedwin[id].maximized == nil or savedwin[id].maximized == false) then
                     savedwin[id].frame = frame
                     savedwin[id].maximized = true
                     win:maximize()
                  else
                     savedwin[id].maximized = false
                     win:setFrame(savedwin[id].frame)
                     savedwin[id] = nil
                  end
end)

-- ** Exposé
-- This is not working 100%
expose = hs.expose.new(nil,{showThumbnails=true}) -- default windowfilter
expose_app = hs.expose.new(nil,{onlyActiveApplication=true}) -- show windows for the current application
expose_space = hs.expose.new(nil,{includeOtherSpaces=false}) -- only windows in the current Mission Control Space

hs.hotkey.bind(mash, 'x', 'Expose',function() expose:toggleShow() end)
hs.hotkey.bind(mashshift, 'x', 'Expose',function() expose_app:toggleShow() end)

--------------------------------------------------------------------------------
-- * Spaces
--------------------------------------------------------------------------------
-- This currently (macOS 10.14) seems to be broken

-- https://gist.github.com/TwoLeaves/a9d226ac98be5109a226

-- Unsupported Spaces extension. Uses private APIs
-- (http://github.com/asmagill/hammerspoon_asm.undocumented)
spaces = require("hs._asm.undocumented.spaces")

currentSpace = tostring(spaces.currentSpace())

-- ** Switch to space
hs.hotkey.bind(mash, '1', function()
                  undo:addToStack()
                  spaces.changeToSpace("1")
end)
hs.hotkey.bind(mash, '2', function()
                  undo:addToStack()
                  spaces.changeToSpace("2")
end)

-- ** Move window to space
hs.hotkey.bind(mashshift, '1', function()
                  undo:addToStack()
                  spaces.moveWindowToSpace(hs.window.focusedWindow():id(), "1")
end)
hs.hotkey.bind(mashshift, '2', function()
                  undo:addToStack()
                  spaces.moveWindowToSpace(hs.window.focusedWindow():id(), "2")
end)

--------------------------------------------------------------------------------
-- * Caffeinate
--------------------------------------------------------------------------------
-- Stolen from
-- https://github.com/songchenwen/dotfiles/blob/master/hammerspoon/init.lua

hs.hotkey.bind(mash, 'c', function() 
      local c = hs.caffeinate
      if not c then return end
      if c.get('displayIdle') or c.get('systemIdle') or c.get('system') then
         if menuCaff then
            menuCaffRelease()
         else
            addMenuCaff()
            local type
            if c.get('displayIdle') then type = 'displayIdle' end
            if c.get('systemIdle') then type = 'systemIdle' end
            if c.get('system') then type = 'system' end
            hs.alert('Caffeine already on for '..type)
         end
      else
         acAndBatt = hs.battery.powerSource() == 'Battery Power'
         c.set('system', true, acAndBatt)
         hs.alert('Caffeinated '..(acAndBatt and '' or 'on AC Power'))
         addMenuCaff()
      end
end)

function addMenuCaff()
   menuCaff = hs.menubar.new()
   menuCaff:setIcon("caffeine-on.pdf") 
   menuCaff:setClickCallback(menuCaffRelease)
end

function menuCaffRelease()
   local c = hs.caffeinate
   if not c then return end
   if c.get('displayIdle') then
      c.set('displayIdle', false, true)
   end
   if c.get('systemIdle') then
      c.set('systemIdle', false, true)
   end
   if c.get('system') then
      c.set('system', false, true)
   end
   if menuCaff then
      menuCaff:delete()
      menuCaff = nil
   end
   hs.alert('Decaffeinated')
end

--------------------------------------------------------------------------------
-- * Load private stuff
--------------------------------------------------------------------------------

dofile_if("private.lua")
