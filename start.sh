#!/bin/bash

# Parameters:  NodeID

nid=$1
if [ "$#" -ne 3 ]; then
    nid=1
fi

unamestr=`uname`
if [[ "$unamestr" == 'Linux' || "$unamestr" == 'Darwin' ]]; then
     exename=erl
else
    exename='start //MAX werl.exe'
    #exename='erl.exe'
fi

# Node
node="-name ipro@127.0.0.1"
node=$node" -setcookie ipro"

# PATHS
paths="-pa"
paths=$paths" _checkouts/*/ebin"
paths=$paths" _build/default/lib/*/ebin"

# erlang opts
erl_opts="-kernel"
erl_opts=$erl_opts" inet_dist_listen_min 8000"
erl_opts=$erl_opts" inet_dist_listen_max 8020"

# lager config
config="-config"
config=$config" console.config"

# sasl opts
sasl_opts="-sasl"
sasl_opts=$sasl_opts" sasl_error_logger false" 

start_opts="$node $paths $erl_opts $sasl_opts $config"

# CPRO start options
echo "------------------------------------------"
echo "Starting IPro (Opts)"
echo "------------------------------------------"
echo "Node      : $node"
echo "Erl Opts  : $erl_opts"
echo "EBIN Path : $paths"
echo "Config    : $config"
echo "SASL      : $sasl_opts"
echo "------------------------------------------"

# Starting ipro
$exename $start_opts -s ipro
