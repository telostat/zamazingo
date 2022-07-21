# zamazingo

![GitHub release (latest by date)](https://img.shields.io/github/v/release/telostat/zamazingo)
![GitHub contributors](https://img.shields.io/github/contributors/telostat/zamazingo)
![GitHub](https://img.shields.io/github/license/telostat/zamazingo)

`zamazingo` is a highly opinionated Haskell package that provides some
helper data definitions and functions. It is not yet another prelude.

We strongly advise against using this in a library. It has
dependencies that library authors probably would not want to bloat
their dependency list with.

Until we reach the first major version, there is NO guarantee over the
API. Indeed, expect many changes.

## Releasing

Below is the release process. Run these under the `nix-shell`.

```sh
git checkout develop
git pull
git checkout main
git pull
git merge --no-ff develop
bash release.sh -n <NEXT-TAG>
git checkout develop
git rebase main
git push
```
