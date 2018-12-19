Param([string]$Arg="list")

Switch ($Arg) {
	"add" {
		$EbinPath="$PSScriptRoot\_build\default\lib\ipro\ebin"
		Write-Host "EbinPath 		: $EbinPath"
		
		$EmuArgs=@(
			"-emu_args",
			"-setcookie", "ipro",
			"-pa", "$EbinPath",
			"-config", "$PSScriptRoot\ipro.config"
			"-s", "ipro"
		)
		$EmuArgs=$EmuArgs -join ' '
		Write-Host "EmuArgs 	  	: $EmuArgs"
		
		$ErlSrvArgs = @(
			"iPro",
			"-comment", "iPro Service",
			"-stopaction", "`"init:stop().`"",
			"-debugtype", "reuse",
			"-env", "ERL_EPMD_PORT=7999",
			"-workdir", "`"$PSScriptRoot`"",
			"-name", "ipro@127.0.0.1",
			"-args", "`"$EmuArgs`""
		)
		Write-Host "ErlSrvArgs		: $ErlSrvArgs"
		
		& "$Env:ERTS_BIN\erlsrv.exe" add $ErlSrvArgs

		break
	}
	"epmd" {
		& "$Env:ERTS_BIN\epmd.exe" -d -port 7999 -names
		break
	}
	"start" {
		& "$Env:ERTS_BIN\erlsrv.exe" start iPro
		break
	}
	"stop" {
		& "$Env:ERTS_BIN\erlsrv.exe" stop iPro
		break
	}
	"remove" {
		& "$Env:ERTS_BIN\erlsrv.exe" remove iPro
		break
	}
	"list" {
		& "$Env:ERTS_BIN\erlsrv.exe" list iPro
		break
	}
	default {
		& "$Env:ERTS_BIN\erlsrv.exe" list iPro
		break
	}
}