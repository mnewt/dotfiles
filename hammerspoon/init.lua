--------------------------------------------------------------------------------
-- Utility functions to convert table to string
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Automatically reload hammerspoon config
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

--------------------------------------------------------------------------------
-- Execute and print to console
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Window tiling
--------------------------------------------------------------------------------

-- hs.hotkey.bind({"cmd", "alt", "ctrl"}, "F", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()
--
--   f.x = max.x
--   f.y = max.y
--   f.w = max.w
--   f.h = max.h
--   win:setFrame(f)
-- end)
--
-- hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()
--
--   f.x = max.x
--   f.y = max.y
--   f.w = max.w / 2
--   f.h = max.h
--   win:setFrame(f)
-- end)
--
-- hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Right", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()
--
--   f.x = max.x + (max.w / 2)
--   f.y = max.y
--   f.w = max.w / 2
--   f.h = max.h
--   win:setFrame(f)
-- end)
--
-- hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Up", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()
--
--   f.x = max.x
--   f.y = max.y
--   f.w = max.w
--   f.h = max.h / 2
--   win:setFrame(f)
-- end)
--
-- hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Down", function()
--   local win = hs.window.focusedWindow()
--   local f = win:frame()
--   local screen = win:screen()
--   local max = screen:frame()
--
--   f.x = max.x
--   f.y = max.y + (max.h / 2)
--   f.w = max.w
--   f.h = max.h / 2
--   win:setFrame(f)
-- end)

--------------------------------------------------------------------------------
-- Check if file exists
--------------------------------------------------------------------------------

function file_exists(name)
  local f=io.open(name,"r")
  if f~=nil then io.close(f) return true else return false end
end

function dofile_if(name)
  if (file_exists(name)) then
    print("Loading " .. name .. "...")
    dofile(name)
  end
end

--------------------------------------------------------------------------------
-- Load stuff
--------------------------------------------------------------------------------

dofile_if("private.lua")
