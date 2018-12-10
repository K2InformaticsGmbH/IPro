$processes = (Get-Process -ProcessName "epmd").ID
foreach ($process in $processes) {Get-Process -PID $process | Stop-Process -Force}