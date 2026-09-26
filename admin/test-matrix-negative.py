"""Exercise the public matrix with a version-only, zero-exit false Emacs."""
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import tempfile

source = Path(__file__).resolve().parent.parent
real = shutil.which(os.environ.get("THANOS_EMACS", "emacs"))
if not real:
    raise SystemExit("A genuine installed Emacs is required for this negative control")
with tempfile.TemporaryDirectory(prefix="yeetube-false-emacs-") as directory:
    fake = Path(directory) / "emacs"
    fake.write_text("#!/bin/sh\ncase \"$*\" in\n"
                    "  *'(princ emacs-version)'*) exec " + shlex.quote(str(Path(real).resolve())) + " \"$@\";;\n"
                    "  *) exit 0;;\nesac\n")
    fake.chmod(0o755)
    env = os.environ.copy()
    for key in ("MAKEFLAGS", "MFLAGS", "MAKEOVERRIDES"):
        env.pop(key, None)
    env["THANOS_EMACS"] = str(fake)
    env["MATRIX_TESTS"] = "test/yeetube-scraper-tests.el"
    result = subprocess.run(["make", "--no-print-directory", "test-matrix"], cwd=source,
                            env=env, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    print(result.stdout, end="")
    match = re.search(r"^Matrix evidence: (.+)$", result.stdout, re.MULTILINE)
    assert match, "Public matrix never started"
    root = Path(match[1])
    assert result.returncode != 0, "False executable was accepted"
    for lane in ("minimum", "default"):
        assert (root / lane / "passed").is_file(), f"Positive {lane} control did not finish"
        stats = json.loads((root / lane / "ert.json").read_text())
        assert stats and all(s["total"] > 0 and s["unexpected"] == 0 for s in stats.values())
    assert not (root / "fork/passed").exists()
    assert not (root / "fork/ert.json").exists()
    assert not list((root / "fork").glob("ert-*.json"))
    assert "PASS all three" not in result.stdout
    print("PASS: public matrix rejects version-only zero-exit Emacs; both Nix controls completed")
