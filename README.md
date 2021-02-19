# asterius-test

A collection of example projects for [Asterius](https://github.com/tweag/asterius/)
that are working as of 2021-02-18.

## Getting started

The easies way to use Asterius is with their prebuilt Docker container, here is
how you do it:

1. Firstly you need to install Docker, just follow the instructions from
   [Docker docs](https://docs.docker.com/get-docker/).
2. Once Docker is up and running, run the Asterius container with the following
   command (this command is always used to enter the container)
   ```sh
   docker run -it -v $(pwd):/workspace -w /workspace terrorjack/asterius
   ```
   Where `$(pwd)` is the absolute path to this directory. Docker will mount this
   directory to the "workspace", where you can use all the Asterius commands.
3. The first time you use the container you must ["boot"](https://asterius.netlify.app/architecture.html#about-booting)
   Asterius, it is also a good idea to update cabal. Do that with these
   commands:
   ```sh
   ahc-boot && ahc-cabal new-update
   ```
   Together they take about 20 minutes, and then you ready.

Well up and running, you should use `ahc-cabal` instead of `cabal` to compile
the project. Building the executable should look like this now:
```sh
ahc-cabal new-build exe:xxx
```
Where `xxx` is the name of the executable you want to build.

But building targeting WASM doesn't actually get us anything useful. To
transform the output to `.wasm` and `.js` you can use `ahc-dist`. First, step
into the directory where you want the output, then run the following command:
```sh
ahc-dist --browser --bundle \
	--output-directory /workspace/web/ \
	--input-exe path/to/xxx
```
The `--input-exe` is the path to the output executable of building the project.
The `--output-directory` argument is an absolute path to where you want the
resulting files, make sure it exists before running the command.

In your output directory you'll now find `xxx.wasm` and `xxx.js`, and an example
HTML file `xxx.html` showing how to use them.

To test the web page, I suggest using [`npm`](https://www.npmjs.com/) and
[`lite-server`](https://www.npmjs.com/package/lite-server), but really any web
server should work. The crux here is that you can not just open `xxx.html` in
your browser. To be able to run the WASM it needs to be server from a server.

Open a new terminal, step into the folder where you placed the output and start
`lite-server`:
```sh
npx lite-server
```
It should open a browser window for you. In it, change the address to access the
generated HTML file. (Probably something like `http://127.0.0.1:3000/xxx.html`.)

Once the web page is open, check the Dev Console for any result.

## Examples

### Hello World

**What**: A simple Hello World Haskell program using `putStdLn` to print to the
JavaScript console.

**Where**: `./hello-world`

**How**: Compile with the following commands (watch out for version changes)
```sh
ahc-cabal new-build exe:hello-world
ahc-dist --browser --bundle \
	--output-directory /workspace/web/ \
	--input-exe ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/asterius-test-0.1.0.0/x/hello-world/opt/build/hello-world/hello-world
```

### Calling JS

**What**: This is the example for calling JavaScript from Haskell provided by
[Tweag on their blog](https://www.tweag.io/blog/2018-09-12-asterius-ffi/). It
shows how JavaScript expressions can be called from Haskell and how the result
can be retrieved.

Notice that the package `asterius-prelude` must be specified in the cabal file
in order to access `Asterius.Types.JSVal`.

**Read more**: [`JSVal` and other shared types](#JSVal-and-other-shared-types)
and [Syntax of `foreign import javascript`](#Syntax-of-foreign-import-javascript)

**Where**: `./calling-js`

**How**: Compile with the following commands (watch out for version changes)
```sh
ahc-cabal new-build exe:calling-js
ahc-dist --browser --bundle \
	--output-directory /workspace/web/ \
	--input-exe ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/asterius-test-0.1.0.0/x/calling-js/opt/build/calling-js/calling-js
```

### Calling JS async

**What**: This example shows how we can call asynchronous JavaScript code, in
the for of a `Promise`, and make Haskell wait for it to resolve.  
It just uses `setTimeout` to wait the specified amount of milliseconds, but more
involved examples can be made.

**Read more**: [Syntax of `foreign import javascript`](#Syntax-of-foreign-import-javascript)
and [Async JavaScript](#Async-JavaScript)

**Where**: `./calling-js-async`

**How**: Compile with the following commands (watch out for version changes)
```sh
ahc-cabal new-build exe:calling-js-async
ahc-dist --browser --bundle \
	--output-directory /workspace/web/ \
	--input-exe ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/asterius-test-0.1.0.0/x/calling-js-async/opt/build/calling-js-async/calling-js-async
```

### Calling HS

**What**: This is the example for calling Haskell from JavaScript provided by
[Tweag on their blog](https://www.tweag.io/blog/2018-09-12-asterius-ffi/). Is
show how to export functions from Haskell, and how to call them from JavaScript.

Watch out! Things do not work as advertised here.

**Read more**: [What entry JavaScript files do](https://asterius.netlify.app/ahc-link.html#input-mjs-arg)

**Where**: `./calling-hs`

**How**: Compile with the following commands.  
Exporting functions does not currently work with `ahc-dist`, see [tweag/asterius#686](https://github.com/tweag/asterius/issues/686),
and we will therefore use `ahc-link` for this.  
Notice the flags for exporting functions `--export-function <function_name>`,
and for setting the entry JavaScript file `--input-mjs <path/to/entry.mjs>`.
```sh
ahc-link --browser --bundle \
	--input-mjs ./calling-hs/calling-hs.mjs \
	--export-function mult_hs \
	--output-prefix calling-hs \
	--output-directory /workspace/web/ \
	--input-hs ./calling-hs/Main.hs
```
Because of `ahc-link` the output files will be called "Main" if not for the
`--output-prefix` argument.

### Passing Objects

**What**: This example shows how we can use [`aeson`](https://hackage.haskell.org/package/aeson)
to pass values, other than te primitives, between Haskell and JavaScript.

Notice that the packages `asterius-prelude` and `aeson` must be specified in the
cabal file.

**Read more**: [Converting Objects](#Converting-Objects)

**Where**: `./passing-objects`

**How**: Compile with the following commands (watch out for version changes)
```sh
ahc-cabal build exe:passing-objects
ahc-dist --browser --bundle \
	--output-directory /workspace/web/ \
	--input-exe ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/asterius-test-0.1.0.0/x/passing-objects/opt/build/passing-objects/passing-objects
```

### Callback

**What**: This example shows how we can pass Haskell functions as callbacks to
JavaScript. A Haskell callback is added to `onClick` event of the document.
Click anywhere on the page to activate it and see the result in the Dev Console.

Notice that the packages `asterius-prelude` must be specified in the cabal file.

**Read more**: [Creating callbacks](#Creating-callbacks)

**Where**: `./callback`

**How**: Compile with the following commands (watch out for version changes)
```sh
ahc-cabal build exe:callback
ahc-dist --browser --bundle \
	--output-directory /workspace/web/ \
	--input-exe ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/asterius-test-0.1.0.0/x/callback/opt/build/callback/callback
```

## Explanations

### `JSVal` and other shared types

When passing values to and from JavaScript, take notice of their type.
Primitives such as `Double`, `Int`, `Char`, and `Bool` can be passed freely. All
other types will be `JSVal`, or one of its `newtype`.

Read [Converting Objects](#Converting-Objects) to learn more about passing
complex data structures between Haskell and JavaScript.

### Syntax of `foreign import javascript`

The syntax for defining JavaScript expressions callable from Haskell looks like
this:

```haskell
foreign import javascript "<expression>" <haskell_name> :: <haskell_type>
```

Where `<expression>` is any JavaScript expression. The type (`<haskell_type>`)
should always return some kind of `IO`, it can be a function taking any number
of ["shared types"](#JSVal-and-other-shared-types) arguments.

Arguments will be inserted into the expression at `$n` symbols. Where `n` is the
one based index of the argument. The same index can be used multiple times.

An example:
```haskell
foreign import javascript "$1 + $1" js_duplicate :: Double -> IO Double
```
This function will take one `Double` and have JavaScript return the duplicated
value.

### Async JavaScript

The syntax for calling asynchronous Javascript is the same as with other
JavaScript, but with the addition of the `safe` keyword,

```haskell
foreign import javascript safe "<expression>" <haskell_name> :: <haskell_type>
```

The JavaScript expression in asynchronous cases must return a `Promise`. The
function will return control to Haskell when the promise is resolved. If the
promise is rejected, a `JSException` will be thrown. Read more about exception
handling [in the documentation](https://asterius.netlify.app/jsffi.html#error-handling-in-jsffi-imports).

### Converting Objects

When passing values between Haskell and JavaScript, they must either be of one
of the [primitive types](#JSVal-and-other-shared-types) or of the reference
type `JSVal`.

To convert a Haskell type to `JSVal` we can use Asterius
`jsonToJSVal` function. For this to work the type must be an instance of
`aeson`s `ToJSON` class.

Like wise we can use Asterius `jsonFromJSVal` to convert from `JSVal` to a
Haskell type. But only if that type is an instance of `aeson`s `FromJSON` class.

Converting to and from `JSVal` is expensive, and doing so should be kept to a
minimum.

The introduction to the `aeson` library on Hackage gives an overview of how to
write instances of `ToJSON` and `FromJSON`. [`Data.Aeson` on Hackage](https://hackage.haskell.org/package/aeson-1.5.5.1/docs/Data-Aeson.html).

### Creating callbacks

Asterius includes a special kind of `foreign import` for "converting" a Haskell
function into a JavaScript function. This syntax works for functions with any
number of `JSVal` (or [primitives](#JSVal-and-other-shared-types)) arguments.
The syntax looks as follows:

```haskell
foreign import javascript "wrapper" js_makeCallback :: (<function_type>) -> IO JSVal
```

Any Haskell function be "converted" into a JavaScript callback, including
partially applied function.

Here follows an example where a function of type `Int -> Int` ic converted into
a JavaScript callback. In JavaScript, callbacks created this way will be
asynchronous and they must be awaited to retrieve their result.

```haskell
foreign import javascript "wrapper" js_makeCallbackIntInt :: (Int -> Int) -> IO JSVal
```

More information about creating callbacks can be found in [the official
documentation](https://asterius.netlify.app/jsffi.html#jsffi-dynamic-exports).
