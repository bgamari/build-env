# build-env <a href="https://hackage.haskell.org/package/build-env" alt="Hackage"><img src="https://img.shields.io/hackage/v/build-env.svg" /></a>

**`build-env`** is a utility to build a set of Cabal packages (computed
by `cabal-install`'s solver) into a free-standing package database.

This enables the compilation of Haskell packages (with their dependencies)
in hermetic build environments.

---

### Contents

- [Example](#example)
- [Commands](#commands)
  - [Parallelism](#parallelism)
- [What does `build-env` do?](#what-does-build-env-do)
- [Hermetic builds](#hermetic-builds)
  - [Narrowing a build down (for debugging)](#narrowing-a-build-down-for-debugging)
- [Bootstrapping](#bootstrapping)
  - [Bootstrap arguments](#bootstrap-arguments)
  - [Ninja](#ninja)
- [Specifying packages](#specifying-packages)
  - [Local packages](#local-packages)

---

## Example

```
$ build-env build lens -f sources -o install -j8
$ ghci -package-db install/package.conf -package lens
ghci> :ty Control.Lens.Lens
Control.Lens.Lens
  :: Control.Lens.Type.Lens s t a b
     -> Control.Lens.Reified.ReifiedLens s t a b
```

In this example, the `build-env build` invocation:

  - computes a build plan for `lens`,
  - fetches `lens` and its dependencies into the `sources` directory,
  - configures and builds all the packages, with up to `8` units building
    concurrently, and registers the libraries into the package database at
    `install/package.conf`.

We then tell `ghci` to use this package database, making `lens` available.

## Commands

`build-env` has three distinct modes of operation:

  - `plan` computes a build plan, outputting a `plan.json` Cabal plan.
  - `fetch` fetches sources.
  - `build` executes a build plan.

Each command subsumes the functionality of the previous ones in the above list.
In particular, in the previous example, the `build` command computed a build
plan, fetched the sources, and built the plan. The same could have been achieved
with three separate invocations:

```
$ build-env plan lens -p lens-plan.json
$ build-env fetch -p lens-plan.json -f sources
$ build-env build -p lens-plan.json -f sources -o install -j8 --prefetched
```

Being able to separate these steps affords us some extra flexibility, as
subsequent sections will explain.

__Note__: it is preferable to pass a previously computed build plan when calling
`build-env build --prefetched`: an invocation of the form `build-env build a b c --prefetched`
will compute a new build plan, and this build plan could be different from
the plan that was previously fetched, for example if you have run `cabal update`
in the meantime.  
Another option is to pass `--index-state`, which pins down a specific Hackage
index state.

### Parallelism

`build-env` supports parallel builds in much the same way as `cabal-install` does:

  - `-jN` controls how many packages are built concurrently.
  - `--configure-arg --ghc-option=-jN` controls the parallelism afforded to
    GHC invocations when building individual packages.
  - `--jsem N` allows `build-env` and GHC to coordinate usage of CPU resources.  
    This is usually the fastest, but it requires:
      - GHC >= 9.8,
      - that GHC and `build-env` have been compiled against the same libc implementation
    (see [GHC issue #25087](https://gitlab.haskell.org/ghc/ghc/-/issues/25087)).

## What does `build-env` do?

- **plan:** `build-env` calls `cabal build --dry-run` on a dummy
  project with the dependencies and constraints that were asked for.  
  This produces a Cabal plan in the form of a `plan.json`, which `build-env`
  parses.  
  **Required executables:** `cabal`, `ghc`.
- **fetch:** `build-env` calls `cabal get` on each package in the build plan.  
  **Required executables:** `cabal`.
- **build:** `build-env` builds the dependencies by compiling their `Setup`
  scripts and calling `Setup configure`, `Setup build`,
  `Setup haddock` (optional), `Setup copy`, and, for libraries, `Setup register`
  and `ghc-pkg register`.  
  **Required executables:** `ghc`, `ghc-pkg`.

Note that, when building packages, `build-env` passes the following information
when running the `Setup` script:

  - `with-compiler`, `prefix` and `destdir` options supplied by the user,
  - `cid`, `datadir`, `datasubdir`, `bindir` and `builddir` options supplied by `build-env`,
  - package flags and `dependency` arguments, obtained from the build plan,
  - `<pkg>_datadir` environment variables supplied by `build-env`.

These options cannot be overridden. If you absolutely need to change some of
these, please file a bug on the issue tracker asking for this customisability.

## Hermetic builds

When working in a hermetic build environment, we might need to compute a build
plan and fetch all the dependencies first, before doing an offline build.

This can be achieved by two separate `build-env` invocations.

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

### Narrowing a build down (for debugging)

When attempting to execute a large build plan, it can be useful to be able
to refine the build down to a small set of problematic packages, for debugging
purposes. For this, you can use `--only`, e.g.:

```
$ build-env build --prefetched -p plan.json -f sources -o install --only badPackage
```

Instead of building the full plan from `plan.json`, this will instead restrict
to only building units from package `badPackage` and their transitive dependencies.  
Multiple `--only` arguments combine additively, e.g. `--only pkg1 --only pkg2`
will build the units from the build plan that belong to `pkg1` or `pkg2`, and
transitive dependencies thereof.

Note that `--only` only affects which packages are built; it does not change
the computation of the build plan nor which packages are fetched.

You can also resume a build where it left off by passing `--resume`; this will
avoid rebuilding any of the units that have already been registered in the
package database.

## Bootstrapping

In the example from [§ Hermetic builds](#hermetic-builds), we ran
`build-env build` to perform a build. However, this requires that the build
environment provision `build-env`.

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

One particularly useful application is the bootstrapping of `build-env` itself,
as this supplies a mechanism for the provisioning of `build-env` in build
environments.

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
$ build-env build -p myPlan.json -f sources -o install --prefetched --configure-arg \$myArgs --script script.sh
```

This will output a script which passes `$myArgs` to each `Setup configure`
invocation. In the build environment, you can then set the value of `$myArgs`
before running the shell script `script.sh`. Note that the build script
__does not__ insert additional quotation marks around `$myArgs`, which allows
passing multiple arguments at once (this is crucial when one doesn't know
the number of arguments ahead of time).

If you want to set `--fetchdir`, `--prefix` or `--destdir` to variables,
you should pass the `--variables` flag. This will set these to the values
`$SOURCES`, `$PREFIX` and `$DESTDIR` in the output shell script, respectively.
These should be set to absolute paths before running the script.  
The script will also use `$GHC` and `$GHCPKG` variables for the compiler.  

### Ninja

`build-env` can also generate [Ninja](https://ninja-build.org/) files,
which support parallel execution (unlike simple shell scripts):

```
$ build-env build lens -f sources -o install --script build_lens.ninja --ninja
$ ninja -f build_lens.ninja
```

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

### Local packages

To specify that a package should be found locally rather than fetched from
Hackage, use `--local`:

```
build-env build myPkg --local myPkg=../pkgs/myPkg
```

The part to the right of the `=` sign corresponds to what you would write
in a `cabal.project` file: the directory in which `myPkg.cabal` is located.

If this is a relative path, it will be interpreted relative to the working
directory of the `build-env` invocation, which can be overriden by passing
`--cwd <dir>` (works like the `-C` argument to `make`).
