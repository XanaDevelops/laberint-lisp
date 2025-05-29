@echo off
set "INPUT=%~1"
set "OUTPUT=%~2"
ffmpeg -y -i "%INPUT%" -c:a pcm_s16le -ar 44100 "%OUTPUT%"
