import os
import subprocess
import argparse


# CLI
parser = argparse.ArgumentParser()

parser.add_argument(
    "--print-err",
    action='store_true', # Acts like a flag
    help="only display errors from tests",
    default=False
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
print(clr_str("\t\tLaunching tests for Rev language", YELLOW))

total_tests = 0
total_ok = 0
total_ko = 0

for dir in os.listdir():
    if dir in ["benchmark", "benchmark_results"] or os.path.isfile(dir):
        continue

    print(f"\n\tTesting folder: {clr_str(dir, YELLOW)}\n")

    files = os.listdir(dir)
    nb_tests = len(files)
    print(f"running {nb_tests} tests")

    total_tests += nb_tests

    for file in files:
        path = f"{dir}\\{file}"

        # Error print mode
        if args.print_err:
            print(f"running {dir}::{file}...")
            
            result = subprocess.run(["..\\target\\debug\\rev.exe", "-f", path], capture_output=True)
            rev_output = result.stdout.decode().strip()

            for line in rev_output.split("\n"):
                if "error" in line or "-->" in line or " | " in line or "^" in line or line == "":
                    print(line)
                else:
                    continue

        else:
            result = subprocess.run(["..\\target\\debug\\rev.exe", "-f", path], capture_output=True)

            content = open(path, "r", encoding="utf-8")

            errors = []
            expects = []
            for line in content.readlines():
                if "error" in line:
                    err = line.split("error: ")[1].strip()
                    errors.append(err)
                elif "expect" in line:
                    exp = line.split("expect: ")[1].strip()
                    expects.append(exp)

            rev_output = result.stdout.decode().strip()

            rev_res = []
            rev_err = []
            for line in rev_output.split("\n"):
                if "-->" in line or " | " in line or "^" in line or line == "":
                    continue
                elif "error" in line:
                    rev_err.append(line.split(": ")[1])
                else:
                    rev_res.append(line.strip())

            ok = rev_res == expects and rev_err == errors

            if ok:
                res = clr_str("Ok", GREEN)
                total_ok += 1
            else:
                res = clr_str("Ko", RED)
                total_ko += 1

            print(f"{clr_str('testing', YELLOW)} {dir}::{file}...  {res}")

            if not ok:
                if len(rev_res) > 0:
                    print(f"Expected:\n{expects}")
                    print(f"Got:\n{rev_res}")

                if len(rev_err) > 0:
                    print(f"Expected errors:\n{errors}")
                    print(f"Got errros:\n{rev_err}")

                print()

print(clr_str("\n\n\t\tStatistics\n", YELLOW))
print(f"Total tests: {total_tests}")
print("Total Ok: " + clr_str(f"{total_ok}", GREEN))
print("Total Ko: " + clr_str(f"{total_ko}", RED))

print("\n")