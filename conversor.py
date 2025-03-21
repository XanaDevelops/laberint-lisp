import os
from PIL import Image

TILE_PATH: str = "tiles/"

paint = lambda count: f"(drawrel {count} 0)"
setColor = lambda color: f"(color {color[0]} {color[1]} {color[2]})"

def conversor(path_in: str, path_out: str) -> None:
    print(f"Convertint: {path_in}")
    with Image.open(path_in, "r") as bmp, open(path_out, "w") as lsp:
        bmp: Image.Image = bmp
        for y in range(bmp.height):
            lastColor: tuple[int, int, int, int] | None = None
            consecutive: int = 0
            for x in range(bmp.width):
                color: tuple[int, int, int, int] = bmp.getpixel((x,y))
                if color != lastColor:
                    if lastColor != None:
                        lsp.write(setColor(lastColor) + paint(consecutive))
                    lastColor = color
                    consecutive = 1
                else:
                    consecutive += 1
                
            lsp.write(setColor(color)+paint(consecutive)+f"(moverel {-bmp.width} -1)\n")

def main() -> None:
    print("conversor .bmp a .lsp para practica laberinto")
    print(f"{TILE_PATH=}")
    for x in os.listdir(TILE_PATH):
        path: str = os.path.join(TILE_PATH, x)
        if os.path.isfile(path) and path.endswith(".bmp"):
            conversor(path, os.path.join(TILE_PATH, x.replace(".bmp", ".lsp")))

if __name__ == "__main__":
    main()