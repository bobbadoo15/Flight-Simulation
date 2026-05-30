@echo off
setlocal
type image.txt

REM ======= COMPILER =======
set GF=gfortran

REM ======= DIRECTORIES =======
set BIN_DIR=bin
set MOD_DIR=mod
set OBJ_DIR=obj
set IN_DIR=input
set OUT_DIR=output
set SRC_DIR=src
set EXE_DIR=%BIN_DIR%\falcon.exe

REM ---- Create bin, obj, and mod if they don't exist ----
if not exist build\%BIN_DIR% mkdir build\%BIN_DIR%
if not exist build\%MOD_DIR% mkdir build\%MOD_DIR%
if not exist build\%OBJ_DIR% mkdir build\%OBJ_DIR%

REM ======= COMPILE FORTRAN FILES =======
REM ---- List files in order ----
set FILES= ^
    %SRC_DIR%\units_m.f90 ^
    %SRC_DIR%\math\vector_m.f90 ^
    %SRC_DIR%\math\matrix_m.f90 ^
    %SRC_DIR%\math\euler_angles_m.f90

REM ---- Loop through each file to compile ----
for %%f in %FILES% do (
    
)

REM ======= LINKER =======

ECHO Build complete!
endlocal