# View registry settings
Get-ItemProperty HKLM:\SYSTEM\CurrentControlSet\Enum\HID\*\*\Device` Parameters FlipFlopWheel -EA 0

# Change registry settings
# Reverse mouse wheel scroll FlipFlopWheel = 1 
# Normal mouse wheel scroll FlipFlopWheel = 0 
Get-ItemProperty HKLM:\SYSTEM\CurrentControlSet\Enum\HID\*\*\Device` Parameters FlipFlopWheel -EA 0 | ForEach-Object { Set-ItemProperty $_.PSPath FlipFlopWheel 1 }
