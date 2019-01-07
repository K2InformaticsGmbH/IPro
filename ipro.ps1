Param([string]$Arg="info")

$InstallRoot="$PSScriptRoot\.."
$Erts="erts-{{erts_vsn}}"
Switch ($Arg) {
	"add" {
		$EbinPath="$InstallRoot\lib\ipro-{{release_version}}\ebin"
		Write-Host "EbinPath 		: $EbinPath"
		
		$EmuArgs=@(
			"-emu_args",
			"-setcookie", "{{cookie}}",
			"-boot", "$InstallRoot\releases\{{release_version}}\ipro",
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
			"-workdir", "`"$InstallRoot\log`"",
			"-name", "{{node}}",
			"-args", "`"$EmuArgs`""
		)
		Write-Host "ErlSrvArgs		: $ErlSrvArgs"
		
		& "$InstallRoot\$Erts\bin\erlsrv.exe" add $ErlSrvArgs

		break
	}
	"console" {
		$env:ERL_EPMD_PORT = {{epmd_port}}
		$env:ERL_EPMD_ADDRESS = "{{host_address}}"
		& "$InstallRoot\$Erts\bin\werl.exe" `
			-boot "$InstallRoot\releases\{{release_version}}\ipro" `
			-config "$InstallRoot\releases\{{release_version}}\ipro.config" `
			-name {{node}} -setcookie {{cookie}} `
		break
	}
	"epmd" {
		& "$InstallRoot\$Erts\bin\epmd.exe" -d -port {{epmd_port}} -names
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
		Write-Host "Usage: $Script [add | remove | start | stop list | console | epmd]"
		break
	}
}