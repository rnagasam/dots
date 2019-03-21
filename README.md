# dots

Personal configuration files.


## Moving to another machine

Run the following commands to clone repository and set up an alias to ease
usage.

```bash
git clone --bare git@github.com:rnagasam/dots.git
alias dots='git --work-tree=$HOME --git-dir=$HOME/dots/'
dots config status.showUntrackedFiles no
```
Finally,

```bash
dots checkout
```

This might fail if certain configuration files exist in `$HOME`, so be
sure to move them some place else.


### Additional steps

To launch emacs as a daemon on startup, run

```bash
systemctl --user enable --now emacs.service
```
Alternatively, run `emacs --daemon` on startup (maybe in `xinitrc`?).

For `offlineimap` and `msmtp`, you need to create the files
`$HOME/.offlineimappass.gpg` and `$HOME/.msmtp-stevens.gpg` containing
passwords for the respective email accounts.  Emacs will automatically
encrypt/decrypt files with a `.gpg' extension, so it's the easiest to
create these from within the editor.
