#!/usr/bin/env sh

rebar3 as test do elvis && rebar3 as cowboy2 do ct
