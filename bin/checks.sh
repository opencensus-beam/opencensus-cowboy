#!/usr/bin/env sh

rebar3 as test do elvis, xref, dialyzer, ct --readable=false
