IPro : IPro Intranet Communication Test Tool
=====

The tool runs as a Windows service (for now) on two or more servers. It can measure networking quality parameters and log communication failures on detection of disruption of existing Erlang cluster.

# Local Build

```sh
$ rebar3 compile
```

# Deployables

A powershell script is included to build deployable for two nodes (minimum number of nodes for an effective iPro setup). The script can be used as follows.

## Build
### Pre-Build
`./config/test_node1.config` and `./config/test_node2.config` MUST be present otherwise the script will fail.

For writing a `./config/nodeX.config` use `./config/dev.config` as template.

### Deploy build script (Works only with GitBash shell)
```sh
# WARNING _build and deploy folders are deleted/cleaned before proceeding
$ powershell.exe -F make_deploy.ps1
```
Steps executed:
1. `rm -rf _build deploy/node*`
1. `rebar3 as node1 tar`
1. `tar xvzf _build/node1/rel/ipro/ipro-X.Y.Z.tar.gz -C ./deploy/node1-ipro-X.Y.Z`
1. `rebar3 as node2 tar`
1. `tar xvzf _build/node2/rel/ipro/ipro-X.Y.Z.tar.gz -C ./deploy/node2-ipro-X.Y.Z`
1. [PS] `Compress-Archive -Path deploy\node1-ipro-X.Y.Z -CompressionLevel Fastest -DestinationPath deploy\node1-ipro-X.Y.Z.zip`
1. [PS] `Compress-Archive -Path deploy\node2-ipro-X.Y.Z -CompressionLevel Fastest -DestinationPath deploy\node2-ipro-X.Y.Z.zip`

Deployable package (`.zip`) are created in `./deploy` folder.
```sh
deploy
  |
  +-- node1-ipro-X.Y.Z.zip
  |
  `-- node2-ipro-X.Y.Z.zip
```

## Installation
1. Unzip `nodeN-ipro-X.Y.Z.zip` into a folder
1. Start Powershell
1. Execute `<IPro Unzip Dir>\nodeN-ipro-X.Y.Z\bin\ipro.ps1 add` to install `iPro` as windows service
1. Execute `<IPro Unzip Dir>\nodeN-ipro-X.Y.Z\bin\ipro.ps1 list` to verify is `iPro` is installed as windows service correctly

## Start / Stop as Windows Service
Start :  `<IPro Unzip Dir>\nodeN-ipro-X.Y.Z\bin\ipro.ps1 start`
Stop: `<IPro Unzip Dir>\nodeN-ipro-X.Y.Z\bin\ipro.ps1 stop`

## Un-Install
1. Start Powershell
1. Execute `<IPro Unzip Dir>\nodeN-ipro-X.Y.Z\bin\ipro.ps1 remove` to un-install `iPro` from windows services
1. Execute `<IPro Unzip Dir>\nodeN-ipro-X.Y.Z\bin\ipro.ps1 list` to verify is `iPro` is un-installed from windows services correctly

## Start as console (_when Windows service is **NOT** running_)
1. Start Powershell
1. Change directory to folder where ipro is unzipped
1. Execute `<IPro Unzip Dir>\nodeN-ipro-X.Y.Z\bin\ipro.ps1 console` to start `iPro` node in erlang windows console

## Configuration
The runtime configuration of iPro can be found at : `<IPro Unzip Directory>nodeN-ipro-X.Y.Z\releases\X.Y.Z\ipro.config`
Current configurations are:
```erlang
[
	{kernel, [
		{inet_dist_listen_min, 8000}, % cluster traffic TCp port range minimum
		{inet_dist_listen_max, 8020}, % cluster traffic TCp port range maximum
		{error_logger, {file, "./ipro_kernel.log"}}
	]},
	{ipro, [
		{other_nodes, ['ipro@...']}, % name of the other ipro node(s) to test the cluster health with
		{idle_check_period, 10000}, % check interval when cluster is healthy
		{error_check_period, 1000} % check interval when cluster is split
	]},
	{sasl, [
		{sasl_error_logger, {file, "./ipro_sasl.log", [append]}} % path to detail erlang debug logs
	]}
].
```

## Command Line Options
```sh
> <IPro Unzip Dir>\nodeN-ipro-X.Y.Z\bin\ipro.ps1
Usage: ipro.ps1 [add | remove | start | stop | list | console | epmd-start | epmd-names | epmd-kill]
```

## Logging
Log files path : `<IPro Unzip Dir>\nodeN-ipro-X.Y.Z\log`
Log Files:
`iPro.debug` - console logs are redirected here when run as service
`ipro_kernel.log` - Service crash log (usually empty)
`ipro_sasl.log` - Erlang verbose debugging logs

### `iPro.debug` log sample on start
```erlang
Executing: <IPro Unzip Dir>\nodeN-ipro-X.Y.Z\erts-9.2\bin\beam.smp.dll
    <IPro Unzip Dir>\nodeN-ipro-X.Y.Z\erts-9.2\bin\beam.smp.dll
    -- -root <IPro Unzip Dir>\nodeN-ipro-X.Y.Z -progname erl
    -- -home C:\WINDOWS\system32\config\systemprofile -epmd_port 7999
    -- -service_event ErlSrv_iPro -nohup
    -name ipro@nodeN -setcookie ipro
    -boot <IPro Unzip Dir>\nodeN-ipro-X.Y.Z\bin\..\releases\X.Y.Z\start
    -config <IPro Unzip Dir>\nodeN-ipro-X.Y.Z\bin\..\releases\X.Y.Z\ipro.config


2019.01.21 16:30:06.006000 [ipro:init:37] <0.75.0> supervisor starting...

2019.01.21 16:30:06.022000 [ipro_worker:init:38] <0.76.0> 
====== [ipro_worker starting] ======
    ERLSRV_SERVICE_NAME : iPro
    ERLSRV_EXECUTABLE   : "<IPro Unzip Dir>\nodeN-ipro-X.Y.Z\erts-9.2\bin\erlsrv.exe"
    ERL_EPMD_PORT       : 7999
    IdleCheckPeriod     : 10000
    ErrorCheckPeriod    : 1000
    OtherNodes          : ['ipro@OTHER_NODE']
    kernel env          : [{included_applications,[]},
                           {inet_dist_listen_max,8020},
                           {inet_dist_listen_min,8000},
                           {error_logger,{file,"./ipro_kernel.log"}}]
    sasl env            : [{sasl_error_logger,
                               {file,"./ipro_sasl.log",[append]}},
                           {included_applications,[]},
                           {errlog_type,all}]
====================================
Eshell V9.2  (abort with ^G)
(ipro@NODE)1> 
```
### `iPro.debug` log lines pattern with healthy cluster
```erlang
...
2019.01.21 16:30:16.037000 [ipro_worker:handle_info:60] <0.76.0> all nodes ['ipro@OTHER_NODE']
    are reachable, next check after : 10000 ms
(ipro@NODE)1> 
2019.01.21 16:30:26.037000 [ipro_worker:handle_info:60] <0.76.0> all nodes ['ipro@OTHER_NODE']
    are reachable, next check after : 10000 ms
(ipro@NODE)1> 
2019.01.21 16:30:36.036000 [ipro_worker:handle_info:60] <0.76.0> all nodes ['ipro@OTHER_NODE']
    are reachable, next check after : 10000 ms
...
```

### `iPro.debug` log line patterns on cluster split
```erlang
...
(ipro@NODE)1>
2019.01.21 16:50:47.642000 [ipro_worker:handle_info:60] <0.76.0> all nodes ['ipro@OTHER_NODE']
    are reachable, next check after : 10000 ms
(ipro@NODE)1>
2019.01.21 16:50:57.704000 [ipro_worker:handle_info:64] <0.76.0>
=============== analyze_report ===============
(ipro@NODE)1> 
2019.01.21 16:50:57.704000 [ipro_worker:analyze_report:86] <0.76.0> OTHER_NODE (1.6.4.7)
    epmd is reachable at port 7999
(ipro@NODE)1> 
2019.01.21 16:50:57.704000 [ipro_worker:net_info:97] <0.76.0> FQDN host (from node) OTHER_NODE
    (1.6.4.7)
(ipro@NODE)1> 
2019.01.21 16:50:57.719000 [ipro_worker:net_info:98] <0.76.0> 
--- inet : gethostname ( ) --->
{ok,"OTHER_NODE"}
<--- inet : gethostname ( ) ---
(ipro@NODE)1> 
2019.01.21 16:50:57.719000 [ipro_worker:net_info:99] <0.76.0> 
--- inet : getifaddrs ( ) --->
{ok,[{"\\DEVICE\\TCPIP_{54E1CCE2-58AA-42FA-A51C-E2929D7291E8}",
      [{flags,[up,broadcast,running,multicast]},
       {addr,{1,6,4,7}},
       {netmask,{255,255,255,0}},
       {broadaddr,{1,6,4,255}},
       {hwaddr,[0,80,86,143,3,158]}]},
     {"\\DEVICE\\TCPIP_{316C49F3-6AD7-43AE-86F1-5A8B6F45C548}",
      [{flags,[up,broadcast,running,multicast]},
       {addr,{1,6,1,2}},
       {netmask,{255,255,255,0}},
       {broadaddr,{1,6,1,255}},
       {hwaddr,[0,80,86,143,69,180]}]},
     ...
     {"\\DEVICE\\TCPIP_{065F0C42-703A-11DE-9954-806E6F6E6963}",
      [{flags,[up,loopback,running]},
       {addr,{0,0,0,0,0,0,0,1}},
       {netmask,{65535,65535,65535,65535,65535,65535,65535,65535}},
       {addr,{127,0,0,1}},
       {netmask,{255,0,0,0}}]}]}
<--- inet : getifaddrs ( ) ---
(ipro@NODE)1> 
2019.01.21 16:50:57.719000 [ipro_worker:net_info:102] <0.76.0> 
--- erl_epmd : names ( ) --->
{ok,[{"ipro",8000}]}
<--- erl_epmd : names ( ) ---
(ipro@NODE)1> 
2019.01.21 16:50:57.719000 [ipro_worker:net_info:104] <0.76.0> 
--- erl_epmd : names ( IpAddr ) --->
{ok,[{"ipro",8000}]}
<--- erl_epmd : names ( IpAddr ) ---
(ipro@NODE)1> 
2019.01.21 16:50:57.719000 [ipro_worker:handle_info:66] <0.76.0> ==============================================
(ipro@NODE)1> 
2019.01.21 16:50:57.719000 [ipro_worker:handle_info:67] <0.76.0> [ERROR] ['ipro@OTHER_NODE']
    nodes were un-reachable, next check after : 1000 ms
...
```

# CHANGELOG
date|version|remark
---|---|---
2019-01-21|0.0.1|first release
