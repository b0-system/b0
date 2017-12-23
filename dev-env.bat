@echo off

set B0_DRIVER_LIBDIR=C:\ocamlms64\lib\site-lib
set __B0_DEV_DIR=%cd%\_boot_b0\v\nop\b

doskey b0=%cd%\_boot_b0/v/nop/b/b0-exe/b0.exe $*
doskey d0=%cd%\_boot_b0/v/nop/b/b0-exe/d0.exe $*
