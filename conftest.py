from pathlib import Path

import hy
  # For the side-effect of allowing import of Hy programs.

import pytest

def pytest_collect_file(parent, file_path):
    if file_path.suffix == ".hy":
        return pytest.Module.from_parent(parent, path=file_path)
