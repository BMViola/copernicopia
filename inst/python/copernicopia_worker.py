"""
Download worker for the copernicopia R package.

cmems_download() runs this script as a separate process, so a download can
always be cancelled: pressing Esc in R kills it, and cmems_abort() (from any R
session) asks it to stop by creating a `stop` file in its control folder.

Usage: python copernicopia_worker.py JOB.json

The job file holds the control folder, the subset function to call, and one
set of subset() arguments per file. Progress is reported on stdout, one event
per line, tab-separated:

    ready
    start   <i>
    done    <i>
    error   <i>   <message>
    cancelled
    finished
"""

import importlib
import json
import logging
import os
import sys
import threading
import time

_lock = threading.Lock()


def emit(*fields):
    line = "\t".join(str(f).replace("\t", " ").replace("\n", " ") for f in fields)
    with _lock:
        sys.stdout.write(line + "\n")
        sys.stdout.flush()


def watchdog(control_dir):
    """
    Twice a second: update the heartbeat file, exit at once if a `stop` file
    has appeared, and exit if the R session that started us has gone (on
    Windows that last check is left to processx's supervisor).
    """
    stop_flag = os.path.join(control_dir, "stop")
    heartbeat = os.path.join(control_dir, "heartbeat")
    parent = os.getppid() if os.name == "posix" else None
    while True:
        if os.path.exists(stop_flag):
            try:
                open(os.path.join(control_dir, "stopped"), "w").close()
                emit("cancelled")
            finally:
                os._exit(3)
        if parent is not None and os.getppid() != parent:
            os._exit(4)
        try:
            with open(heartbeat, "w") as f:
                f.write(str(time.time()))
        except OSError:
            pass
        time.sleep(0.5)


def main(job_path):
    with open(job_path, encoding="utf-8") as f:
        job = json.load(f)
    control_dir = job["control_dir"]
    with open(os.path.join(control_dir, "worker.pid"), "w") as f:
        f.write(str(os.getpid()))
    threading.Thread(target=watchdog, args=(control_dir,), daemon=True).start()

    # The toolbox's own log lines and progress bars are not wanted here
    logging.getLogger("copernicusmarine").setLevel(logging.WARNING)
    for path in job.get("python_path", []):
        sys.path.insert(0, path)
    module_name, function_name = job["subset_function"].split(":")
    subset = getattr(importlib.import_module(module_name), function_name)

    emit("ready")
    for i, task in enumerate(job["tasks"]):
        emit("start", i)
        try:
            subset(**task)
            emit("done", i)
        except Exception as e:  # reported to R, which logs it and moves on
            msg = str(e).strip()
            emit("error", i, f"{type(e).__name__}: {msg}" if msg else type(e).__name__)
    emit("finished")


if __name__ == "__main__":
    main(sys.argv[1])
