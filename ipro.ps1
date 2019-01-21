Param([string]$Arg="info")

$InstallRoot="$PSScriptRoot\.."
$Erts="erts-{{erts_vsn}}"
$ErlIniPath="$InstallRoot/$Erts/bin"
$ErlIniContent=@"
[erlang]
Bindir=$ErlIniPath
Progname=erl
Rootdir=$InstallRoot
"@

Switch ($Arg) {
	"add" {
		# Rewrite erts/erl.ini with installation path
		$ErlIniContent > "$ErlIniPath/erl.ini"

		$EbinPath="$InstallRoot\lib\ipro-{{release_version}}\ebin"
		Write-Host "EbinPath 		: $EbinPath"
		
		$EmuArgs=@(
			"-emu_args",
			"-setcookie", "{{cookie}}",
			"-boot", "$InstallRoot\releases\{{release_version}}\start",
			"-config", "$InstallRoot\releases\{{release_version}}\ipro.config"
		)
		$EmuArgs=$EmuArgs -join ' '
		Write-Host "EmuArgs 	  	: $EmuArgs"
		
		$ErlSrvArgs = @(
			"iPro",
			"-comment", "iPro Service",
			"-stopaction", "`"init:stop().`"",
			"-debugtype", "reuse",
			"-env", "ERL_EPMD_PORT={{epmd_port}}",
			"-env", "ERL_EPMD_ADDRESS={{host_address}}",
			"-workdir", "`"$InstallRoot\log`"",
			"-name", "{{node}}",
			"-args", "`"$EmuArgs`""
		)
		Write-Host "ErlSrvArgs		: $ErlSrvArgs"
		
		& "$InstallRoot\$Erts\bin\erlsrv.exe" add $ErlSrvArgs

		break
	}
	"console" {
		# Rewrite erts/erl.ini with installation path
		$ErlIniContent > "$ErlIniPath/erl.ini"

		$env:ERL_EPMD_PORT = {{epmd_port}}
		$env:ERL_EPMD_ADDRESS = "{{host_address}}"
		& "$InstallRoot\$Erts\bin\werl.exe" `
			-boot "$InstallRoot\releases\{{release_version}}\start" `
			-config "$InstallRoot\releases\{{release_version}}\ipro.config" `
			-name {{node}} -setcookie {{cookie}}
		break
	}
	"epmd-start" {
		& "$InstallRoot\$Erts\bin\epmd.exe" -d -d -d -d `
			-address {{host_address}} `
			-port {{epmd_port}}
		break
	}
	"epmd-names" {
		& "$InstallRoot\$Erts\bin\epmd.exe" -d -port {{epmd_port}} -names
		break
	}
	"epmd-kill" {
		& "$InstallRoot\$Erts\bin\epmd.exe" -d -port {{epmd_port}} -kill
		break
	}
	"start" {
		& "$InstallRoot\$Erts\bin\erlsrv.exe" start iPro
		break
	}
	"stop" {
		& "$InstallRoot\$Erts\bin\erlsrv.exe" stop iPro
		break
	}
	"remove" {
		& "$InstallRoot\$Erts\bin\erlsrv.exe" remove iPro
		break
	}
	"list" {
		& "$InstallRoot\$Erts\bin\erlsrv.exe" list iPro
		break
	}
	default {
		$Script= $MyInvocation.MyCommand.Name
		Write-Host "Usage: $Script [add | remove | start | stop | list | console | epmd-start | epmd-names | epmd-kill]"
		break
	}
}