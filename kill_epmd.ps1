Param([Int]$port=0)
Try {
	$processes = (Get-NetTCPConnection -LocalPort $port -RemoteAddress 0.0.0.0 -State "Listen" -ErrorAction Stop).OwningProcess
	foreach ($process in $processes) {
		Get-Process -PID $process | Stop-Process -Force
		Write-Host "epmd pid $process killed"
	}
}
Catch {}