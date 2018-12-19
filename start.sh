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

# PATHS
paths="-pa"
paths=$paths" _checkouts/*/ebin"
paths=$paths" _build/default/lib/*/ebin"

# erlang opts
erl_opts="-kernel"
erl_opts=$erl_opts" inet_dist_listen_min 8000"
erl_opts=$erl_opts" inet_dist_listen_max 8020"

# sasl opts
sasl_opts="-sasl"
sasl_opts=$sasl_opts" sasl_error_logger false" 

start_opts="$paths $erl_opts $sasl_opts"

# CPRO start options
echo "------------------------------------------"
echo "Starting IPro (Opts)"
echo "------------------------------------------"
echo "Erl Opts  : $erl_opts"
echo "Cookie    : $cookie"
echo "EBIN Path : $paths"
echo "SASL      : $sasl_opts"
echo "------------------------------------------"

# Starting ipro
$exename $start_opts -s ipro