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
-- Mount mc:/data when on home network
--------------------------------------------------------------------------------

wifiWatcher = nil
homeSSID = "rageSunset"
lastSSID = hs.wifi.currentNetwork()

function ssidChangedCallback()
   newSSID = hs.wifi.currentNetwork()

   if newSSID == homeSSID and lastSSID ~= homeSSID then
      -- We just joined our home WiFi network
      hs.execute("$HOME/code/hammerspoon/mnt_smb $HOME/private/config/knosis_data")
   elseif newSSID ~= homeSSID and lastSSID == homeSSID then
      -- We just departed our home WiFi network
      hs.execute("$HOME/code/hammerspoon/umnt $HOME/mnt/data")
   end

   lastSSID = newSSID
end

wifiWatcher = hs.wifi.watcher.new(ssidChangedCallback)
wifiWatcher:start()

--------------------------------------------------------------------------------
-- Mount a:/mnt/share when on JUSTIS VPN
--------------------------------------------------------------------------------

vpnGateway = "10.3.231.15"

function vpnChangedCallback(self, flags)
   -- note that because having an internet connection at all will show the remote network
   -- as "reachable", we instead look at whether or not our specific address is "local" instead
   if (flags & hs.network.reachability.flags.isLocalAddress) > 0 then
      -- VPN tunnel is up
      hs.execute("$HOME/code/hammerspoon/mnt_smb_ssh $HOME/private/config/justis_share")
   else
      -- VPN tunnel is down
      hs.execute("$HOME/code/hammerspoon/umnt_smb_ssh $HOME/private/config/justis_share")
   end
end

hs.network.reachability.forAddress(vpnGateway):setCallback(vpnChangedCallback):start()
