import argparse
import socket
import threading
import subprocess
from typing import Dict, Tuple

# Diccionario de canciones (índice -> ruta al archivo WAV)
songs: Dict[int, str] = {
    1: "music\\fireboy and watergirl.wav",
    2: "music\\waterfall.wav",
    3: "music\\win.wav",
    4: "music\\key.wav",
    5: "music\\menu.wav"

}

# Configuración de socket
HOST: str = '127.0.0.1'
PORT: int = 666
BUFFER_SIZE: int = 1024

# Almacena procesos de reproducción activos (índice -> subprocess.Popen)
players: set[subprocess.Popen] = set()

# Variables globales de servidor
server_socket: socket.socket
server_thread: threading.Thread
running: bool = False


def handle_client(client_sock: socket.socket, addr: Tuple[str, int]) -> None:
    """Maneja los comandos recibidos de clientes."""
    global running
    with client_sock:
        while running:
            try:
                data = client_sock.recv(BUFFER_SIZE).decode('utf-8').strip().split()
                if not data:
                    break
                cmd = data[0].upper()
                if cmd == 'PLAY' and len(data) >= 2:
                    idx = int(data[1])
                    loop_flag = (len(data) >= 3 and data[2].lower() == 'loop')
                    play_song(idx, loop_flag)
                    client_sock.sendall(f"Playing {idx}{' with loop' if loop_flag else ''}\n".encode())
                elif cmd == 'STOP':
                    stop_all()
                    client_sock.sendall(b"Stopped all songs\n")
                elif cmd == 'KILL':
                    kill_server()
                    client_sock.sendall(b"Server killed\n")
                    break
                else:
                    client_sock.sendall(b"Unknown command\n")
            except Exception as e:
                client_sock.sendall(f"Error: {e}\n".encode())
                break


def play_song(index: int, loop: bool = False) -> None:
    """
    Inicia la reproducción de la canción indicada por su índice.
    Si loop es True, se reproduce en bucle.
    """
    if index not in songs:
        raise ValueError(f"Índice {index} no encontrado en canciones.")
    path = songs[index]
    # Construir comando PowerShell
    if loop:
        ps_command = [
            "powershell", "-Command",
            f"while($true){{(New-Object Media.SoundPlayer '{path}').PlaySync()}}"
        ]
    else:
        ps_command = [
            "powershell", "-Command",
            f"(New-Object Media.SoundPlayer '{path}').PlaySync()"
        ]
    print(f"cancion {index=}")
    proc = subprocess.Popen(ps_command)
    players.add(proc)


def stop_all() -> None:
    """Detiene todas las reproducciones en curso usando taskkill."""
    for proc in list(players):
        try:
            subprocess.run([
                "taskkill", "/PID", str(proc.pid), "/T", "/F"
            ], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        except Exception:
            pass
    players.clear()


def start_server() -> None:
    """Inicia el servidor de sockets."""
    global server_socket, server_thread, running
    if running:
        print("Server ya está en ejecución.")
        return
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.bind((HOST, PORT))
    server_socket.listen()
    running = True
    print(f"Server escuchando en {HOST}:{PORT}")

    def run() -> None:
        while running:
            try:
                client, addr = server_socket.accept()
                threading.Thread(target=handle_client, args=(client, addr), daemon=True).start()
            except OSError:
                break

    server_thread = threading.Thread(target=run, daemon=True)
    server_thread.start()


def kill_server() -> None:
    """Detiene el servidor y libera recursos."""
    global running
    running = False
    try:
        server_socket.close()
    except Exception:
        pass
    stop_all()
    print("Server detenido.")


def send_command(command: str) -> None:
    """Envía un comando al servidor y muestra la respuesta."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.connect((HOST, PORT))
        sock.sendall(command.encode('utf-8'))
        response = sock.recv(BUFFER_SIZE).decode('utf-8')
        print(response.strip())


def main() -> None:
    parser = argparse.ArgumentParser(description='Cliente/Servidor de canciones vía sockets')
    parser.add_argument(
        'mode', choices=['server', 'play', 'stop', 'kill'],
        help="'server' para iniciar, 'play', 'stop' o 'kill' para cliente"
    )
    parser.add_argument('-i', '--index', type=int, help='Índice de la canción para play')
    parser.add_argument('-l', '--loop', action='store_true', help='Loop para play')
    args = parser.parse_args()

    if args.mode == 'server':
        start_server()
        try:
            while running:
                threading.Event().wait(1)
        except KeyboardInterrupt:
            kill_server()
    else:
        if args.mode == 'play':
            if args.index is None:
                print("Error: --index es requerido para play.")
            else:
                cmd = f"PLAY {args.index}{' loop' if args.loop else ''}"
                send_command(cmd)
        elif args.mode == 'stop':
            send_command("STOP")
        elif args.mode == 'kill':
            send_command("KILL")


if __name__ == '__main__':
    main()
