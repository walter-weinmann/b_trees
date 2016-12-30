### 1. Test Environment

All files related to **common test** and **eunit** can be found in the directory `test`. The test components in directory `test` are:

- the **common test** suites `b_trees_SUITE.erl` and `performance_SUITE.erl`,
- the **eunit** test file `b_trees_test.erl`.

Both `b_trees_SUITE.erl` and `b_trees_test.erl` apply the same tests. Furthermore, the following files have been adjusted:

- `.gitignore`: defines the test beam files and the test result files to be ignored,
- `.travis.yml`: adds a `./rebar3 ct` command (`after success`),
- `rebar.config`: defines the parameters for **common test**, **coverage analysis** and **eunit**. 

### 2. Running Tests

**Common Test**, **eunit** and **coverage analysis** can all be executed from the command line with the appropriate `rebar3` commands. The execution of **common test** is also included in Travis CI.

### 3. Checking Test Results

The results of the **common test** execution can be checked in the following file:
 
    file:///.../b_trees/_build/test/logs/index.html

The results of the **coverage analysis** can be checked in the following file:
 
    file:///.../b_trees/_build/test/cover/index.html

### 4 Travis CI

**Common Test** is currently configured as an `after success` script in Travis CI. This has the disadvantage, that the build of **b_trees** is marked as successful even if **common test** fails.  

