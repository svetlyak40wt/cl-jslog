# JSON log formatter

This is a command line tool for JSON logs formatting and filtering.

I know about `jq`, but it is too complex for my simple task.

## How to run

Get a compiled binary for your platform.

Run as:

tail -f some.log | jslog <filter> <format>

## How to compile

TODO: write instruction

## How to test

To run a full testsuite from console:

```
./run-tests.sbcl
```

To run a single test from repl:

* start Emacs;
* load content of `.local.el`;
* in opened repl enter:

  ```
  (st:test :test 'cl-jslog-tests::test-filter-from-broken-string)
  ```

