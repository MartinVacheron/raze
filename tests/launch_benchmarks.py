import os
import subprocess
import time
import argparse


# CLI
parser = argparse.ArgumentParser()

parser.add_argument(
    "--msg",
    help="add a message at the top of benchmark results file",
)

args = parser.parse_args()

# ANSI escape codes
RED = "\033[31m"
GREEN = "\033[32m"
YELLOW = "\033[33m"
BLUE = "\033[34m"
RESET = "\033[0m"

def clr_str(msg: str, clr: str) -> str:
    return f"{clr}{msg}{RESET}"


# Main
print(clr_str("\n\t\tLaunching benchmarks for Rev language\n", YELLOW))

# Compilation
print("Compiling in release mode...")
subprocess.run(["cargo", "build", "--release"], cwd="..", capture_output=True)
print("Compilation done\n")

# Result file
time = time.strftime("%Y%m%d-%H%M%S")
outfile = open(f"benchmark_results\\benchmark_{time}", "w")

if not args.msg is None:
    outfile.write(f"Benchmark message: \n{args.msg}\n\n")

for dir in os.listdir():
    if dir == "benchmark":
        files = os.listdir(dir)

        for file in files:
            path = f"{dir}\\{file}"
            print(f"running benchmark: {dir}::{file}...")
            outfile.write(f"running benchmark: {dir}::{file}...\n")

            result = subprocess.run(["..\\target\\release\\rev.exe", "-f", path], capture_output=True)

            rev_output = result.stdout.decode().strip()

            for line in rev_output.split("\n"):
                print(line)
                outfile.write(f"\t{line}\n")

            print()
            outfile.write("\n")

        print()
        outfile.close()

        exit()