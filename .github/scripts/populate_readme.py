import json
from os.path import exists
from tabulate import tabulate
with open("original.json", "r") as f:
    pkgs = json.load(f)
import requests

tables = {"Failed": [], "Unclaimed": [], "Succeeded": []}

for pkg in list(pkgs):
    name = pkg
    runid = ""
    runurl = ""
    status = "Unclaimed"
    tarname = ""
    plog = ""
    if exists(f"logs/run_ids/{pkg}"):
        with open(f"logs/run_ids/{pkg}", "r") as frun:
            runid = frun.read()
    if "https://github.com/" in runid:
        runurl = runid.strip().replace("null\n", "").split("\n")[-1]
        r = requests.get(runurl)
        while r.status_code not in [404, 200]:
            r = requests.get(runurl)
        if r.status_code == 404:
            runurl = runurl.replace("gha-build", "gha-build-old")
            while r.status_code not in [404, 200]:
                r = requests.get(runurl)
        if r.status_code == 200:
            name = f"[{pkg}]({runurl})"
    if exists(f"lists/failed/{pkg}"):
        status = "Failed"
        tarname = f"https://github.com/almahmoud/gha-build/blob/main/lists/failed/{pkg}"
    elif exists(f"lists/{pkg}"):
        with open(f"lists/{pkg}", "r") as pf:
            plog = pf.read()
    if plog.endswith("tar.gz\n"):
        status = "Succeeded"
        tarname = plog.strip()
        tarname = f"[{tarname}](https://js2.jetstream-cloud.org:8001/swift/v1/gha-build/{tarname})"
    tables[status].append([name, status, tarname])\

headers = ["Package", "Status", "Tarball"]
failedheaders = ["Package", "Status", "Log"]
with open("README.md", "w") as f:
    f.write(f"# Summary\n\n{len(tables['Succeeded'])} built packages\n\n{len(tables['Failed'])} failed packages\n\n{len(tables['Unclaimed'])} unclaimed packages\n\n")
    f.write(f"\n\n## Failed ({len(tables['Failed'])})\n")
    f.write(tabulate(tables["Failed"], failedheaders, tablefmt="github"))
    f.write(f"\n\n## Succeeded ({len(tables['Succeeded'])})\n")
    f.write(tabulate(tables["Succeeded"], headers, tablefmt="github"))
    f.write(f"\n\n## Unclaimed ({len(tables['Unclaimed'])})\n")
    f.write(tabulate(tables["Unclaimed"], headers, tablefmt="github"))
