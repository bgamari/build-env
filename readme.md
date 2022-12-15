# build-env <a href="https://hackage.haskell.org/package/build-env" alt="Hackage"><img src="https://img.shields.io/hackage/v/build-env.svg" /></a>

**`build-env`** is a utility to build a set of Cabal packages (computed
by `cabal-install`'s solver) into a free-standing package database.

## Example

```
$ build-env build lens -f sources -o install -j8
$ ghci -package-db install/package.conf/ -package lens
GHCi, version 9.0.2: https://www.haskell.org/ghc/  :? for help
λ> :ty Control.Lens.Lens
Control.Lens.Lens
  :: Control.Lens.Type.Lens s t a b
     -> Control.Lens.Reified.ReifiedLens s t a b
```

In this example, the `build-env build` invocation:

  - computes a build plan for `lens`,
  - fetches `lens` and its dependencies into the `sources` directory,
  - configures, builds and registers all the libraries into the `install`
    directory, with up to `8` units building concurrently.

We can then tell `ghci` to use this package database.

## Deferred builds

In some situations (e.g. in hermetic build environments), we might need to
compute a build plan and fetch all the dependencies first, before doing
an offline build.

This can be achieved with `build-env` with the following two separate
invocations.

In a local environment (with access to internet):

```
$ build-env fetch lens -f sources --output-plan plan.json
```

In a build environment in which `build-env` has been provisioned:

```
$ build-env build --prefetched -p plan.json -f sources -o install -j8
```

In this example:

  - the `fetch` command computes a plan which is written to
    `plan.json` (using the Cabal JSON plan format), and fetches all
    the required sources, putting them into the `sources` directory;
  - the `build` command reads in the build plan and performs the build.

## Bootstrapping

In the above example, we ran `build-env build` to perform a build. However,
this requires that the build environment provision `build-env`.

To avoid this requirement, it is also possible to ask `build-env` to output
a shell script containing the build steps to execute.

In a local environment (with access to internet):

```
$ build-env build lens -f sources -o install --script build_lens.sh
```

In a build environment:

```
$ ./build_lens.sh
```

The downside is that we lose any parallelism from this approach, as the
generated build script is inherently sequential.

### Bootstrap arguments

If your build environment provides additional variables that need to be passed
to the build script, you can pass them to the shell script using variables.

For example, in the local environment, you can first obtain all the information
needed to return a build script:

```
$ build-env fetch lens -f sources --output-plan myPlan.json
```

Next, compute a build script containing references to variables:

```
$ build-env build -p myPlan.json -f sources -o install --prefetched --configure-arg $myArgs --script script.sh
```

This will output a script which passes `$myArgs` to each `Setup configure`
invocation. In the build environment, you can then set the value of `$myArgs`
before running the shell script `script.sh`. Note that the build script
__does not__ insert additional quotation marks around `$myArgs`, which allows
passing multiple arguments at once (this is crucial when one doesn't know
the number of arguments ahead of time).

## Specifying packages

Instead of passing the required packages through the command line,
it can be more convenient to ask `build-env` to read them from files:

```
$ build-env build --seeds SEEDS --freeze cabal.config -f sources -o install
```

This will build a plan for the packages specified in the `SEEDS` file,
with versions of packages that aren't in the `SEEDS` file constrained as
specified by the `cabal.config` file.

Each line in the seeds file should be one of the following:

  - A Cabal unit, in the format `unit +flag1 -flag2 >= 0.1 && < 0.3`.

    A unit can be of the form `pkgName`, `lib:pkgName`, `exe:pkgName`,
    `pkgName:lib:compName`, ... as per `cabal` component syntax.

    The unit name must be followed by a space.

    Flags and constraints are optional.  
    When both are present, flags must precede constraints.  
    Constraints must use valid Cabal constraint syntax.

  - An allow-newer specification such as:

    ```
    allow-newer: pkg1:pkg2,pkg3:base,*:ghc
    ```

    This uses Cabal `allow-newer` syntax.

  - An empty line or comment (starting with `--`).

The `cabal.config` file should use valid `cabal.project` syntax, e.g.:

```cabal
constraints: pkg1 == 0.1,
             pkg2 >= 3.0
```

Only the constraints are processed; everything else from the `cabal.config`
file is ignored.
