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
hs.hotkey.bind(mash, "V", function() hs.eventtap.keyStrokes(hs.pasteboard.getContents()) end)

--------------------------------------------------------------------------------
-- * Automatically reload hammerspoon config
--------------------------------------------------------------------------------

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
myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Config loaded")

hs.hotkey.bind(mash, 'r', reloadConfig)

--------------------------------------------------------------------------------
-- * Window tiling
--------------------------------------------------------------------------------

hs.window.animationDuration = 0 -- disable animations

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "F", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w
      f.h = max.h
      win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h
      win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Right", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x + (max.w / 2)
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h
      win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Up", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w
      f.h = max.h / 2
      win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Down", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y + (max.h / 2)
      f.w = max.w
      f.h = max.h / 2
      win:setFrame(f)
end)

--------------------------------------------------------------------------------
-- Spaces
--------------------------------------------------------------------------------

-- https://gist.github.com/TwoLeaves/a9d226ac98be5109a226

-- Unsupported Spaces extension. Uses private APIs but works okay.
-- (http://github.com/asmagill/hammerspoon_asm.undocumented)
spaces = require("hs._asm.undocumented.spaces")

--------------------------------------------------------------------------------
-- * Window moving and resizing
--------------------------------------------------------------------------------
-- Credit to GitHub user: ztomer

hs.grid.MARGINX = 0
hs.grid.MARGINY = 0
hs.grid.GRIDHEIGHT = 18
hs.grid.GRIDWIDTH = 18

--Alter gridsize
hs.hotkey.bind(mashshift, '=', function() hs.grid.adjustHeight( 1) end)
hs.hotkey.bind(mashshift, '-', function() hs.grid.adjustHeight(-1) end)
hs.hotkey.bind(mash, '=', function() hs.grid.adjustWidth( 1) end)
hs.hotkey.bind(mash, '-', function() hs.grid.adjustWidth(-1) end)

--Snap windows
hs.hotkey.bind(mash, ';', function() hs.grid.snap(hs.window.focusedWindow()) end)
hs.hotkey.bind(mash, "'", function() hs.fnutils.map(hs.window.visibleWindows(), hs.grid.snap) end)

--Move windows
hs.hotkey.bind(mash, 'j', hs.grid.pushWindowDown)
hs.hotkey.bind(mash, 'k', hs.grid.pushWindowUp)
hs.hotkey.bind(mash, 'h', hs.grid.pushWindowLeft)
hs.hotkey.bind(mash, 'l', hs.grid.pushWindowRight)

--resize windows
hs.hotkey.bind(mashshift, 'k', hs.grid.resizeWindowShorter)
hs.hotkey.bind(mashshift, 'j', hs.grid.resizeWindowTaller)
hs.hotkey.bind(mashshift, 'l', hs.grid.resizeWindowWider)
hs.hotkey.bind(mashshift, 'h', hs.grid.resizeWindowThinner)

-- toggle window zoom (acts like Alt+Shift+GreenPlusButton)
hs.hotkey.bind(mash, "m", function()
                  local win = hs.window.focusedWindow()
                  local frame = win:frame()
                  local id = win:id()

                  -- init table to save window state
                  savedwin = savedwin or {}
                  savedwin[id] = savedwin[id] or {}

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

--------------------------------------------------------------------------------
-- * Spaces
--------------------------------------------------------------------------------
-- This currently (macOS 10.14) seems to be broken

currentSpace = tostring(spaces.currentSpace())

-- ** Switch to space
hs.hotkey.bind(mash, '1', function() spaces.changeToSpace("1") end)
hs.hotkey.bind(mash, '2', function() spaces.changeToSpace("2") end)

-- ** Move window to space
hs.hotkey.bind(mashshift, '1', function() spaces.moveWindowToSpace(hs.window.focusedWindow():id(), "1") end)
hs.hotkey.bind(mashshift, '2', function() spaces.moveWindowToSpace(hs.window.focusedWindow():id(), "2") end)

--------------------------------------------------------------------------------
-- * Load private stuff
--------------------------------------------------------------------------------

dofile_if("private.lua")
