@echo off
echo Compiling Binary Memory Analyzer...
g++ -std=c++17 -O2 -o memory_analyzer.exe memory_analyzer.cpp
if %ERRORLEVEL% EQU 0 (
    echo Compilation successful!
    echo Running memory analyzer...
    memory_analyzer.exe
) else (
    echo Compilation failed!
)
