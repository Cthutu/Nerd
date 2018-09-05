@echo off
if [%INSTALL_PATH%] NEQ [] (
    copy /y _bin\Win64_Release_nd\nd.exe %INSTALL_PATH%
    copy /y _bin\Win64_Release_nd\*.n %INSTALL_PATH%
) else (
    echo Please define INSTALL_PATH in the environment
)

