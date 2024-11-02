# Installation

Mewlix's compiler is a self-contained binary executable. It has no external dependencies.

The easiest way to install it is to download the **pre-compiled binaries** available [here][3]!

### Linux

1. Download the `mewlix` binary executable.
2. Copy it to `~/.local/bin`:
```bash
chmod +x mewlix
cp mewlix ~/.local/bin/
```
3. Add `~/.local/bin` to your PATH if it hasn't already been added.

### Windows

The simplest way to install/use Mewlix on Windows is: 

1. Download the `mewlix.exe` executable.
2. Place it in a new directory.
3. Add that directory to your PATH. See [this StackOverflow answer][4] for some help with that.

If you wish to temporarily add the directory to your PATH for a running `cmd` instance, you can do:
```cmd
set "PATH=%PATH%;C:\your-mewlix-folder\"
```
These changes will only apply to that instance of `cmd`, and will not affect the system as a whole.

> "Wait, can't I just use SETX to permanently change my PATH variable with Batch/CMD?"

You *can*, but I don't think you *should*. [This StackOverflow answer explains why][5].

### Build From Source

1. Install [stack][2]. You should preferably get version 2.13.1 or above.
2. Clone the [mewlix][1] repository:
```bash
git clone https://github.com/kbmackenzie/mewlix
cd mewlix
```
3. Build the `mewlix` binary and install it to `~/.local/bin` with `stack`:
```bash
stack setup
stack install
```
4. Add `~/.local/bin` to your PATH if it hasn't already been added.

[1]: https://github.com/kbmackenzie/mewlix
[2]: https://docs.haskellstack.org/en/stable/
[3]: https://github.com/kbmackenzie/mewlix/releases/latest
[4]: https://stackoverflow.com/a/44272417/19764270
[5]: https://stackoverflow.com/a/69239861/19764270
