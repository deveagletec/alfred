![](https://img.shields.io/github/license/deveagletec/alfred.svg) ![](https://img.shields.io/github/stars/deveagletec/alfred.svg) ![](https://img.shields.io/github/forks/deveagletec/alfred.svg) ![](https://img.shields.io/github/tag/deveagletec/alfred.svg) ![](https://img.shields.io/github/release/deveagletec/alfred.svg) ![](https://img.shields.io/github/issues/deveagletec/alfred.svg) ![](https://img.shields.io/github/last-commit/deveagletec/alfred.svg)
## Commands
1. **[config](#config)**
2. **[new](#new)**
3. **[init](#init)**
4. **[install](#install)**
5. **[update](#update)**
6. **[uninstall](#uninstall)**
7. **[show](#show)**
8. **[generate](#generate)**
9. **[destroy](#destroy)**
10. **[db:migrate](#db:migrate)**
11. **[db:update](#db:update)**

### config
```bash
alfred config
```
### new
```bash
alfred new
```
### init
```bash
alfred init
```
### install
The `install` command reads the `package.json` file from the current directory, resolves the dependencies, and installs them into `vendor`.
```bash
alfred install
```
If there is a `package.lock` file in the current directory, it will use the exact versions from there instead of resolving them. This ensures that everyone using the library will get the same versions of the dependencies.

If there is no `package.lock` file, Composer will create one after dependency resolution.
#### Options
- ** --save-prod (-P):** Package will appear in your dependencies.
- ** --save-dev (-D):** Package will appear in your devDependencies.
- ** --force (-f):** Forces overwriting of files.
- ** --global (-g):** The -g or --global argument will cause alfred to install the package globally rather than locally.

### update
```bash
alfred update
```
### uninstall
```bash
alfred uninstall
```
### show
The `show` command list all of the available packages.

```bash
alfred show
```
To filter the list you can pass a package mask using wildcards.
```bash
alfred show delphi/*
```
If you want to see the details of a certain package, you can pass the package name.
```bash
alfred show delphi/log
```
#### Options
- **--installed (-i):** List the packages that are installed
- **--name (-n):** List package names
- **--version (-v):** List package versions
- **--self (-s):** List the root package info

### generate
### destroy
### db:migrate
### db:update