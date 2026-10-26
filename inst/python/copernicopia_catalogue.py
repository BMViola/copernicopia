"""
Catalogue fetcher for the copernicopia R package.

cmems_catalogue() runs this script as a separate process, so R can show its
own timer while the catalogue is fetched (the toolbox's progress bars do not
display well in the R console) and Esc can stop it.

Usage: python copernicopia_catalogue.py OUT.json [module:function [path ...]]

Writes {"columns": ..., "skipped": [...]} (see catalogue_table() in
copernicopia_helpers.py) to OUT.json. The optional module:function replaces
copernicusmarine:describe, importable from the extra paths (used in tests).
"""

import importlib
import json
import logging
import os
import sys

import copernicopia_helpers as helpers


def main(out_path, describe_function="copernicusmarine:describe", *python_path):
    logging.getLogger("copernicusmarine").setLevel(logging.WARNING)
    for path in python_path:
        sys.path.insert(0, path)
    module_name, function_name = describe_function.split(":")
    describe = getattr(importlib.import_module(module_name), function_name)
    result = helpers.catalogue_table(disable_progress_bar=True, describe=describe)
    partial = out_path + ".part"
    with open(partial, "w", encoding="utf-8") as f:
        json.dump(result, f)
    os.replace(partial, out_path)


if __name__ == "__main__":
    main(*sys.argv[1:])
