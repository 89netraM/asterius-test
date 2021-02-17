# asterius-test

A collection of example projects for [Asterius](https://github.com/tweag/asterius/)
that are working as of 2021-02-17.

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
the project, we've also have a flag for activating the WASM features.
Building the executable should look like this now:
```sh
ahc-cabal new-build exe:xxx --flags="wasm"
```

But building targeting WASM doesn't actually get us anything useful. To
transform the output to `.wasm` and `.js` you can use `ahc-dist`. First, step
into the directory where you want the output, then run the following command:
```sh
ahc-dist --input-exe path/to/xxx --browser --bundle --output-directory /workspace/web/
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

## Hello World

**What**: A simple Hello World Haskell program using `putStdLn` to print to the
JavaScript console.

**Where**: `./hello-world`

**How**: Compile with the following commands (watch out for version changes)
```sh
ahc-cabal new-build exe:hello-world
ahc-dist --input-exe ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/asterius-test-0.1.0.0/x/hello-world/opt/build/hello-world/hello-world --browser --bundle --output-directory /workspace/web/
```

## Calling JS

**What**: This is the example for calling JavaScript from Haskell provided by
[Tweag on their blog](https://www.tweag.io/blog/2018-09-12-asterius-ffi/). It
shows how JavaScript expressions can be called from Haskell and how the result
can be retrieved.

When passing values to and from JavaScript, take notice of their type.
Primitives such as `Double`, `Int`, `Char`, and `Bool` can be passed freely. All
other types will be `JSVal`, or one of its `newtype`.

Notice that the package `asterius-prelude` must be specified in the cabal file
in order to access `Asterius.Types.JSVal`.

**Where**: `./calling-js`

**How**: Compile with the following commands (watch out for version changes)
```sh
ahc-cabal new-build exe:calling-js
ahc-dist --input-exe ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/asterius-test-0.1.0.0/x/calling-js/opt/build/calling-js/calling-js --browser --bundle --output-directory /workspace/web/
```
