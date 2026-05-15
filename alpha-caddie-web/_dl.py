import urllib.request
from pathlib import Path
url = "https://raw.githubusercontent.com/jriordan55/alpha-caddie/3133f6c/alpha-caddie-web/app.js"
out = Path(__file__).resolve().parent / "app.js"
data = urllib.request.urlopen(url, timeout=120).read()
out.write_bytes(data)
print(len(data))
