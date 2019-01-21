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

## Configuration

## Command Line Options

## Starting as Service

## Logging

# CHANGELOG
date|version|remark
---|---|---
2019-01-21|0.0.1|first release
