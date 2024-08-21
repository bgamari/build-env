# Changelog for `build-env`

## Version 1.2.0.0 (2024-08-21)

- Allow specifying a Hackage index state using the `--index-state` flag (@simonmar).

- Avoid using process substitution in generated build scripts, allowing the
  build scripts to be run in environments in which `/dev/fd` is not available (@simonmar).

- Add support for the GHC `-jsem` flag (GHC 9.8 and above), for speeding up parallel builds.

- Add support for relocatable builds using `--configure-arg --enable-relocatable`.

- Relax upper bounds on various dependencies of `build-env`.

## Version 1.1.0.0 (2023-03-09)

- Change the default directory structure produced by `build-env` to be more
  predictable.

- Ensure that important arguments to Cabal configure override arguments
  passed using `--configure-arg`, as they are essential for `build-env`
  to function.

## Version 1.0.0.0 (2022-12-26)

- First released version.

## Version 0.1.0.0 (2022-11-16)

- Initial proof of concept.
