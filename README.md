## Common actions

***How to run locally:***

1. Clone the source code

`git clone https://github.com/Igorocky/fe-template.git`

2. Navigate to the project's directory

`cd fe-template`

3. Install npm dependencies

`npm install`

4. Compile ReScript code (local and from all npm dependencies) to JS code

`npm run compile-with-deps`

5. Run the project locally

`npm run start`

***How to run all unit tests***

`npm run compile-test-all-unit`

***How to run all integration tests***

`npm run compile-test-all-int`


***How to run a particular test***

1. In package.json, update 'compile-test-single' script replacing put_test_name_here with required test name. For example:

replace: `"compile-test-single": "rescript && mocha --timeout 10000000 -g \"'put_test_name_here'\" src/**/test/**/*.js",`

with: `"compile-test-single": "rescript && mocha --timeout 10000000 -g \"'finds proofs for simple wffs'\" src/**/test/**/*.js",`

2. Run tests:

`npm run compile-test-single`

***How to debug***

There is no standard way to debug ReScript code. But ReScript gets compiled to readable JS code. So it is possible to debug JS code. In order to debug any use case:
1. Create a test which reproduces required use case or modify existing test.
2. Compile the code `npm run compile`
3. Use your favorite IDE to place a break point in generated JS code and then run tests in debug mode.
