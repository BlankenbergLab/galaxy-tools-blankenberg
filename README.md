# galaxy-tools-blankenberg
Contains a set of Galaxy Tools written by the Blankenberg Lab

## Continuous integration

GitHub Actions run Planemo linting and tests for changed tool repositories on
pull requests and pushes to `main` or `master`. A scheduled workflow also runs
the full repository weekly. Both workflows follow the same Planemo CI action
pattern used by `tools-iuc` and `galaxytools-bgruening`.

For local checks, install Planemo and use the Makefile targets:

```sh
python -m pip install planemo
make lint
make test
```

To check a single repository:

```sh
make lint TOOL_DIRS=tools/column_regex_substitution
make test TEST_DIRS=tools/column_regex_substitution
```
