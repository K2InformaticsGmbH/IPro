#!/bin/sh
echo "cleanup : rm -rf _build deploy/*"
rm -rf _build deploy/node*

echo "release extract node1"
rebar3 as node1 tar
mkdir -p ./deploy/node1-ipro-0.0.2
tar xvzf _build/node1/rel/ipro/ipro-0.0.2.tar.gz -C ./deploy/node1-ipro-0.0.2

echo "release extract node2"
rebar3 as node2 tar
mkdir -p ./deploy/node2-ipro-0.0.2
tar xvzf _build/node2/rel/ipro/ipro-0.0.2.tar.gz -C ./deploy/node2-ipro-0.0.2