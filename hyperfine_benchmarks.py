import glob
import subprocess
from datetime import date 


today = date.today()
date = today.strftime("%m%d")
label = subprocess.check_output(["git", "rev-parse", "HEAD"]).strip().decode('ascii')[:8]

print(f"Date: {date} | hash: {label}")
subprocess.call(["cargo", "build", "--release"])
for bench in glob.glob("test/benchmark/*"):
    name = bench.split("/")[-1].split(".")[0]
    print(f"Running hyperfine for {name} | {bench}")
    code = subprocess.call(["hyperfine", "--export-csv", "tmp_res.csv", f"target/release/rlox {bench}"])
    with open(f"results/hyperfine/{name}/res.csv", "a") as f:
        line = open("tmp_res.csv").readlines()[1]
        line = f"{date},{label}," + line
        f.write(line)
    subprocess.call(["rm", "tmp_res.csv"])