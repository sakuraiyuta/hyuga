#!/usr/bin/env bash

python3 setup.py sdist
python3 setup.py bdist_wheel
twine upload \
  --repository-url https://pypi.org/legacy/ \
  dist/*

