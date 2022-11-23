# terra-bucket-metrics
Print Google Cloud Storage metrics for buckets associated with Terra workspaces.

The following metrics are supported:
- storage.googleapis.com/storage/object_count
- storage.googleapis.com/storage/total_bytes

# Quick Start
If you're new to Haskell, don't worry - it's a glorious world where we humbly
offer programs to our glorious leader, Simon Peyton Jones. You are most welcome.

## Requirements
- [stack](https://docs.haskellstack.org/en/stable/), A Haskell build tool
- An installation of an implementation of the C mysqlclient API, like [mariadb](https://mariadb.org/)

## Getting Stack
The simplest way to start off building and using Haskell programs with
stack (avoiding questionable decisions some linux distributions make about
packaging haskell libraries) is to install [ghcup](https://www.haskell.org/ghcup/)
and follow the instructions to install the recommended stack and cabal binaries.

## Building
After you've cloned this repository and installed `stack`, `cd` into
`terra-bucket-metrics` and run

    $ stack build

This will install a compatible version of GHC (the Glasgow Haskell Compiler),
fetch and build dependencies and then compile the program. Optionally, you can
run `stack install` afterwards to install the binary to your local bin path
(see output of `stack path --local-bin`).

## Usage
See output of

    $ terra-workspace-metrics --help

Note: You need to define the `VAULT_ADDR` environment variable in the form
`https://hostname:port` and have a valid vault token at `$HOME/.vault-token`.
