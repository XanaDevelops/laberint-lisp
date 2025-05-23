#!/usr/bin/env python3
"""
Script for silent music control server over a local socket on Windows.

Usage:
  music_control.py --start    # Inicia de forma silenciosa sin ventana
  music_control.py --play <track_id> <loop>
  music_control.py --stop
  music_control.py --kill
"""
import argparse
import socket
import threading
import sys
import winsound
import os
import ctypes

# Constants para ocultar ventana de consola
SW_HIDE = 0
SW_SHOW = 5

def hide_console():
    """Oculta la ventana de la consola en Windows."""
    hwnd = ctypes.windll.kernel32.GetConsoleWindow()
    if hwnd:
        ctypes.windll.user32.ShowWindow(hwnd, SW_HIDE)


def detach_console():
    """Desvincula el proceso de cualquier consola inmediatamente al iniciar."""
    ctypes.windll.kernel32.FreeConsole()

# Dictionary mapping track IDs to WAV file paths
TRACKS = {
    1: r'music\\rickroll hq.wav',
    2: r'C:\Music\song2.wav',
    3: r'C:\Music\song3.wav',
}


HOST = '127.0.0.1'
PORT = 666


def start_server(host=HOST, port=PORT):
    """Inicia el servidor silencioso de control de música."""
    #hide_console()
    detach_console()
    stop_event = threading.Event()

    def handle_client(conn):
        data = conn.recv(1024).decode().strip()
        conn.close()
        if data == 'KILL':
            stop_event.set()
            return
        parts = data.split(':')
        try:
            cmd = int(parts[0])
        except ValueError:
            return

        if cmd == -1:
            winsound.PlaySound(None, winsound.SND_PURGE)
        elif cmd in TRACKS:
            loop = len(parts) == 2 and parts[1].lower() in ('true', '1', 'yes')
            flags = winsound.SND_FILENAME | winsound.SND_ASYNC
            if loop:
                flags |= winsound.SND_LOOP
            winsound.PlaySound(TRACKS[cmd], flags)

    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind((host, port))
        s.listen()
        while not stop_event.is_set():
            s.settimeout(1.0)
            try:
                conn, _ = s.accept()
            except socket.timeout:
                continue
            threading.Thread(target=handle_client, args=(conn,), daemon=True).start()
        s.close()
        winsound.PlaySound(None, winsound.SND_PURGE)


def send_command(message, host=HOST, port=PORT):
    """Envía un comando silencioso al servidor de música."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.connect((host, port))
        s.sendall(message.encode())


def main():
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument('--start', action='store_true')
    parser.add_argument('--play', nargs=2, metavar=('TRACK_ID', 'LOOP'))
    parser.add_argument('--stop', action='store_true')
    parser.add_argument('--kill', action='store_true')
    args = parser.parse_args()

    if args.start:
        start_server()
    elif args.play:
        track_id, loop_flag = args.play
        send_command(f"{int(track_id)}:{loop_flag}")
    elif args.stop:
        send_command(str(-1))
    elif args.kill:
        send_command('KILL')
    else:
        parser.print_help()

if __name__ == '__main__':
    main()
