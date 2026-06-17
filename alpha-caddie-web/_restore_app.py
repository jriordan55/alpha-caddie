import subprocess
from pathlib import Path
root = Path(__file__).resolve().parent.parent
blob = subprocess.check_output(
    ["git", "show", "HEAD:alpha-caddie-web/app.js"],
    cwd=str(root),
)
Path(__file__).resolve().parent.joinpath("app.js").write_bytes(blob)
print("wrote", len(blob))
