#!/bin/bash
if test "$OS" = "Windows_NT"
then
  # use .Net

  .paket/paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi
  
  [ ! -e build.fsx ] && .paket/paket.exe update
  [ ! -e build.fsx ] && packages/FAKE/tools/FAKE.exe init.fsx
  packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
else
  # fix test fsproj file
  mv tests/FsPretty.Tests/FsPretty.Tests.fsproj tests/FsPretty.Tests/FsPretty.Tests.fsproj.vs
  mv tests/FsPretty.Tests/FsPretty.Tests.fsproj.mono tests/FsPretty.Tests/FsPretty.Tests.fsproj  
    
  # use mono
  mono .paket/paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  mono .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  [ ! -e build.fsx ] && mono .paket/paket.exe update
  [ ! -e build.fsx ] && mono packages/FAKE/tools/FAKE.exe init.fsx
  mono packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 

  # put project files back to avoid git noticing the swap
  mv tests/FsPretty.Tests/FsPretty.Tests.fsproj tests/FsPretty.Tests/FsPretty.Tests.fsproj.mono
  mv tests/FsPretty.Tests/FsPretty.Tests.fsproj.vs tests/FsPretty.Tests/FsPretty.Tests.fsproj

fi
