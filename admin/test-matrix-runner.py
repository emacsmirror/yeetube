"""Small, runtime-independent regressions for the matrix's trust boundary."""
from contextlib import redirect_stdout
import io
import json
import os
from pathlib import Path
import runpy
import subprocess
import tempfile
import unittest
from unittest.mock import patch

R = runpy.run_path(str(Path(__file__).with_name("test-matrix")))
SOURCE = Path(__file__).resolve().parent.parent


class MatrixTests(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory(prefix="yeetube-runner-")
        self.addCleanup(temp.cleanup)
        self.root = Path(temp.name)

    def test_minimum(self):
        self.assertEqual(R["minimum_version"](SOURCE), "29.1")

    def test_receipt_validation(self):
        good = dict(total=1, completed=1, expected=1, unexpected=0, skipped=0)
        R["validate_stats"](good)
        for change in ({"total": 0}, {"completed": 0}, {"unexpected": 1},
                       {"total": True}, {"expected": -1}):
            with self.subTest(change=change), self.assertRaises(ValueError):
                R["validate_stats"](good | change)

    def test_manifest_and_snapshot(self):
        source = self.root / "source"
        R["snapshot"](SOURCE, source)
        (source / "private.elc").write_text("private")
        R["snapshot"](source, self.root / "copy")
        self.assertFalse((self.root / "copy/private.elc").exists())
        (source / "test/omitted-tests.el").write_text("ordinary suite")
        with self.assertRaisesRegex(ValueError, "every ordinary"):
            R["snapshot"](source, self.root / "rejected")
        (source / "test/omitted-tests.el").unlink()
        (source / "yeetube.el").unlink()
        (source / "yeetube.el").symlink_to(source / "private.elc")
        with self.assertRaisesRegex(ValueError, "symlinked"):
            R["snapshot"](source, self.root / "symlink")

    def test_attempt_all_and_pre_nix_capture(self):
        for missing, status, receipt in ((False, 0, True), (True, 0, True),
                                         (False, 1, True), (False, 0, False)):
            calls, resolved = [], []
            def resolve(command):
                resolved.append(command)
                if missing and command == "user-fork":
                    raise ValueError("missing fork")
                return "/resolved/" + command
            def run(command, **_):
                self.assertEqual(resolved[0], "user-fork")
                name = command[command.index("--lane") + 1]
                calls.append(name)
                root = Path(command[command.index("--root") + 1])
                if receipt:
                    root.mkdir(parents=True)
                    (root / "passed").write_text("29.1\n")
                return subprocess.CompletedProcess(command, status if name == "minimum" else 0)
            root = self.root / str((missing, status, receipt))
            root.mkdir()
            with patch.dict(os.environ, {"THANOS_EMACS": "user-fork"}, clear=True), \
                 patch.dict(R["matrix"].__globals__, {"executable": resolve}), \
                 patch("tempfile.mkdtemp", return_value=str(root)), \
                 patch("subprocess.check_output", return_value=json.dumps({"path": str(SOURCE)})), \
                 patch("subprocess.run", side_effect=run), redirect_stdout(io.StringIO()):
                if missing or status or not receipt:
                    with self.assertRaisesRegex(RuntimeError, "Required matrix lanes failed"):
                        R["matrix"](SOURCE)
                else:
                    R["matrix"](SOURCE)
            self.assertEqual(calls, ["minimum", "default"] if missing else ["minimum", "default", "fork"])

    def test_isolation_fresh_bytecode_and_ert(self):
        deps = self.root / "deps"
        deps.mkdir()
        observed = []
        def run(command, cwd, env, **_):
            observed.append((cwd, env.copy()))
            self.assertNotIn("MAKEFLAGS", env)
            if "compile" in command:
                for file in R["manifest"](cwd, "SRCS"):
                    self.assertFalse((cwd / (file + "c")).exists())
                    (cwd / (file + "c")).write_text("bytecode")
            if "do-matrix-test" in command:
                self.assertIn("TESTS=test/yeetube-scraper-tests.el", command)
                Path(env["YEETUBE_MATRIX_RECEIPT"]).write_text(json.dumps(
                    dict(total=1, completed=1, expected=1, unexpected=0, skipped=0)))
            return subprocess.CompletedProcess(command, 0)
        with patch.dict(os.environ, {"YEETUBE_MATRIX_DEPS": str(deps),
                                     "MATRIX_TESTS": "test/yeetube-scraper-tests.el", "MAKEFLAGS": "bad"}), \
             patch("subprocess.check_output", return_value="29.1"), \
             patch("subprocess.run", side_effect=run), redirect_stdout(io.StringIO()):
            for name in ("minimum", "default", "fork"):
                R["lane"](SOURCE, self.root / name, name, "/fake/emacs", "29.1")
        for key in ("HOME", "TMPDIR", "XDG_CACHE_HOME", "XDG_CONFIG_HOME", "XDG_DATA_HOME", "XDG_STATE_HOME"):
            self.assertEqual(len({env[key] for _, env in observed}), 3)
        self.assertEqual(len({cwd for cwd, _ in observed}), 3)

    def test_wrong_patch_version(self):
        deps = self.root / "deps"
        deps.mkdir()
        with patch.dict(os.environ, {"YEETUBE_MATRIX_DEPS": str(deps)}), \
             patch("subprocess.check_output", return_value="29.4"), \
             patch("subprocess.run") as run, redirect_stdout(io.StringIO()):
            with self.assertRaisesRegex(ValueError, "expected 29.1, got 29.4"):
                R["lane"](SOURCE, self.root / "minimum", "minimum", "/fake/emacs", "29.1")
            run.assert_not_called()


if __name__ == "__main__":
    unittest.main()
