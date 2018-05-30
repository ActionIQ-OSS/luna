#!/bin/bash
./pants publish.jar --local=~/.m2/repository --no-dryrun src/scala/co/actioniq/luna/:luna
