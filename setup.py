import subprocess
import shutil
import sys

print("Setting up fortranbox environment...")

if shutil.which("mamba"):
   cmd = ["mamba", "env", "create", "-f", "setup/setup_conda/environment.yml"]
elif shutil.which("conda"):
   cmd = ["conda", "env", "create", "-f", "setup/setup_conda/environment.yml"]
else:
   sys.exit("Neither conda nor mamba found.")

try:
   subprocess.run(cmd, check=True)
except subprocess.CalledProcessError:
   print("Environment may already exist. Skipping creation.")

print("\nDone. To activate, run:\nconda activate fortranbox")
