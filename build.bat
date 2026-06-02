@echo off
setlocal
:: DISPLAYING PROGRAM IMAGE AT BEGINNING OF COMPILING
type image.txt

:: ============================================================
::          Flight-Simulation Build, Compile, and Run
::                     GFortran 90 | OOP
:: ============================================================

:: ======= COMPILER =======
set GF=gfortran

:: ======= FLAGS =======


:: ======= DIRECTORIES =======
set BIN_DIR=bin
set MOD_DIR=mod
set OBJ_DIR=obj
set IN_DIR=input
set OUT_DIR=output
set SRC_DIR=src
set EXE_DIR=%BIN_DIR%\falcon.exe

:: ---- Create bin, obj, and mod if they don't exist ----
if not exist build\%BIN_DIR% mkdir build\%BIN_DIR%
if not exist build\%MOD_DIR% mkdir build\%MOD_DIR%
if not exist build\%OBJ_DIR% mkdir build\%OBJ_DIR%

:: ======= COMPILE FORTRAN FILES =======
:: ---- Display messages for compilation ----
echo.
echo ============================================================
echo  Compiling modules...
echo ============================================================
echo.

:: ---- List files in order ----
set FILES= ^
    %SRC_DIR%\units_m.f90 ^
    %SRC_DIR%\math\vector_m.f90 ^
    %SRC_DIR%\math\matrix_m.f90 ^
    %SRC_DIR%\math\euler_angles_m.f90
    %SRC_DIR%\falcon.f90

:: ---- Loop through each file to compile ----
for %%f in %FILES% do (
    
)

:: ======= LINKER =======
echo.
echo ============================================================
echo  Linking...
echo ============================================================
echo.

:: ======= RUN =======
echo.
echo ============================================================
echo  Running FlightSim...
echo ============================================================
echo.
%EXE%

:: ======= COMPLETE PROGRAM =======
echo.
echo ============================================================
echo  Build Complete!
echo ============================================================
echo.

endlocal