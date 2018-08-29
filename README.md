# Miscellaneous cheatsheet

This document has a number of tips and tricks relevant to Emacs
usage. It assumes that you are using macOS, but most of the things
will be the same on Linux (other than using a different package
manager, of course). Windows is kind of a lost cause :(

## Command-line navigation

Access the command line by opening Terminal.app, found in
`/Applications/Utilities/Terminal.app`. You can type in a command and
press enter (`RET`) to run it. Most people write terminal commands in
this document by putting them after a `$`, but you don't type the `$`
when you input the command.

Ask which directory you are in:

    $ pwd
    /Users/raxod502/files/school

Show the files in your current directory (note that this is a
lowercase `l`, not a `1`):

    $ ls -lA
    total 0
    drwxr-xr-x  7 raxod502  staff  238 Mar 10 15:40 bhs
    drwxr-xr-x  6 raxod502  staff  204 Nov 14 12:58 hmc
    drwxr-xr-x  6 raxod502  staff  204 Mar 10 16:27 old
    drwxr-xr-x  2 raxod502  staff   68 Mar 10 16:27 temp

Change directory (you can give a name listed by `ls -lA`, the symbol
`..` to move up a directory, or a full path name):

    $ pwd
    /Users/raxod502/files/school
    $ cd hmc
    $ pwd
    /Users/raxod502/files/school/hmc
    $ cd ..
    $ pwd
    /Users/raxod502/files/school
    $ cd /Users/raxod502
    $ pwd
    /Users/raxod502

## Installing Emacs

Ask which program will be run when you type `emacs`:

    $ which emacs
    /usr/bin/emacs

The programs in `/usr/bin` come with macOS, and are usually
outdated. You can install up-to-date versions into `/usr/local/bin`
using Homebrew. Check if you have Homebrew installed:

    $ which brew
    /usr/local/bin/brew

If not (i.e. the command above didn't show the path to a program),
install it by running the command on [their website][homebrew]. Then,
you can install Emacs:

    $ brew install emacs --with-cocoa

You can then ask the shell to refresh its database of program
locations, which is necessary if you had previously run the `emacs` in
`/usr/bin`:

    $ hash -r

You should now see that an up-to-date Emacs is installed:

    $ which emacs
    /usr/local/bin/emacs

Compare the versions of the two (in the latter command, you can use
just `emacs` instead of `/usr/local/bin/emacs` because that is the
default once you have installed the new version and either started a
new shell or run `hash -r`):

    $ /usr/bin/emacs --version
    GNU Emacs 22.1.1
    Copyright (C) 2007 Free Software Foundation, Inc.
    GNU Emacs comes with ABSOLUTELY NO WARRANTY.
    You may redistribute copies of Emacs
    under the terms of the GNU General Public License.
    For more information about these matters, see the file named COPYING.

    $ /usr/local/bin/emacs --version
    GNU Emacs 26.1
    Copyright (C) 2018 Free Software Foundation, Inc.
    GNU Emacs comes with ABSOLUTELY NO WARRANTY.
    You may redistribute copies of GNU Emacs
    under the terms of the GNU General Public License.
    For more information about these matters, see the file named COPYING.

To make it so that you can launch Emacs from the Applications folder
rather than just the command line, you should first check where it was
installed to by running `brew info emacs` and checking for the line
ending in an asterisk:

    $ brew info emacs
    emacs: stable 25.3 (bottled), devel 26.1-rc1, HEAD
    GNU Emacs text editor
    https://www.gnu.org/software/emacs/
    /usr/local/Cellar/emacs/26.0.91 (3,992 files, 128.8MB)
      Built from source on 2018-04-09 at 14:01:16 with: --with-cocoa --with-modules
    /usr/local/Cellar/emacs/26.1-rc1 (3,992 files, 129.2MB) *
      Built from source on 2018-04-16 at 19:39:23 with: --with-cocoa --with-modules
    From: https://github.com/Homebrew/homebrew-core/blob/master/Formula/emacs.rb

As you can see, there are lots of files in that directory. One of them
is the application:

    $ ls -lA /usr/local/Cellar/emacs/26.1-rc1
    total 112
    drwxr-xr-x  3 raxod502  admin    102 May 16 08:35 .brew
    -rw-r--r--  1 raxod502  staff  35149 Apr  9 13:15 COPYING
    -rw-r--r--  1 raxod502  staff   1118 Apr  9 13:01 ChangeLog
    drwxr-xr-x  3 raxod502  staff    102 May 16 08:34 Emacs.app
    -rw-r--r--  1 raxod502  admin   1036 May 16 08:35 INSTALL_RECEIPT.json
    -rw-r--r--  1 raxod502  staff   6122 Apr  5 03:55 README
    drwxr-xr-x  7 raxod502  admin    238 May 16 08:35 bin
    -rw-r--r--  1 raxod502  admin    412 May 16 08:35 homebrew.mxcl.emacs.plist
    drwxr-xr-x  3 raxod502  admin    102 May 16 08:34 lib
    drwxr-xr-x  3 raxod502  admin    102 May 16 08:35 libexec
    drwxr-xr-x  8 raxod502  admin    272 May 16 08:34 share

Then you can copy the application from there, and it will be runnable
from the Applications folder:

    $ cp -R /usr/local/Cellar/emacs/26.1-rc1/Emacs.app /Applications

Start Emacs:

    $ emacs

Start Emacs inside the terminal (some features may not work, but some
people prefer it this way):

    $ emacs -nw

## Configuring Emacs

When Emacs starts up, it loads the *init-file*, which is a file called
`init.el` inside the directory `.emacs.d` in your home directory,
i.e. `~/.emacs.d/init.el`. You can see if you have a `.emacs.d`
directory by running `ls -lA ~`, and check its contents with `ls -lA
~/.emacs.d`. To install this Emacs configuration, you can first delete
the existing `~/.emacs.d` directory (be careful, since when you ask
the shell to delete something, it *really* deletes it):

    $ rm -rf ~/.emacs.d

(If you prefer to be more careful, you can `brew install trash` and
then use `trash ~/.emacs.d` instead, to move things to the trash
instead of deleting them outright.)

Then you want to install the contents of this Git repository to
`~/.emacs.d`, as follows:

    $ git clone https://github.com/raxod502/org-emacs.git ~/.emacs.d

You can run `ls -lA ~/.emacs.d` to verify the results.

## Using Git

First make sure you are in the Git repository (this will work with any
repository, but as an example I'm considering the `org-emacs` one):

    $ cd ~/.emacs.d

Then you can ask for the current status; if all is normal, you will
see:

    $ git status
    On branch master
    Your branch is up to date with 'origin/master'.

    nothing to commit, working tree clean

You will probably want to make changes to your configuration, and
publish them to your own private repository on GitHub. You can do that
by creating a *fork* from the web interface. Make sure to grab the
[GitHub Student Pack][student-pack] so you can create private
repositories for free. After you create the fork (make sure to *not*
check the boxes to add a README or LICENSE), you can tell Git about
it. Git tracks remote repositories via a list of *remotes*. You can
list them:

    $ git remote
    origin

This means there is one remote, called `origin`. You can ask it what
web page that remote corresponds to:

    $ git remote get-url origin
    https://github.com/raxod502/org-emacs.git

By default, when you run `git clone` on a URL, the resulting
repository will have one remote, called `origin`, with that URL. You
want to add another remote (you can call it anything) for your fork:

    $ git remote add <remote-name> https://github.com/<username>/org-emacs.git

We'll see how to use this in a bit. First, you want to have a change
to add to your fork. Git only takes action when you ask it to; your
regular workflow of editing code goes like usual. Once you have
changed a file, the status report will show that:

    $ git status
    On branch master
    Your branch is up to date with 'origin/master'.

    Changes not staged for commit:
            modified:   README.md
            modified:   init.el

    no changes added to commit

The way Git works, you first tell it which changes you want to
include, and then *commit* that set of the changes. You can add (or
*stage*) the changes in a file with `git add`, and un-add (or
*unstage*) with `git reset`. Thus,

    $ git add init.el

    $ git status
    On branch master
    Your branch is up to date with 'origin/master'.

    Changes to be committed:
            modified:   init.el

    Changes not staged for commit:
            modified:   README.md

    $ git add README.md

    $ git status
    On branch master
    Your branch is up to date with 'origin/master'.

    Changes to be committed:
            modified:   README.md
            modified:   init.el

    $ git reset README.md
    Unstaged changes after reset:
    M       README.md

    $ git status
    On branch master
    Your branch is up to date with 'origin/master'.

    Changes to be committed:
            modified:   init.el

    Changes not staged for commit:
            modified:   README.md

Once you have decided which changes you would like to record in the
history, run `git commit` to open an editor for your commit
message. But before doing so, you need to tell Git your name and email
address (for inclusion in the version history), as well as to use
Emacs for editing commit messages:

    $ git config --global user.name "<your first and last name>"
    $ git config --global user.email "<your email address>"
    $ git config --global core.editor emacs

Once you are in your editor, simply type in a message describing the
changes that you are committing ([here are some style conventions for
doing so][commit-messages]). Then save the file and exit the editor,
and your commit will be recorded. If you change your mind, you can
exit with no message, and this will cancel the commit.

You can show a list of all commits to date via:

    $ git log
    commit 1edd89c72a51187eabc8ad7b4c84c9daaa434998 (HEAD -> master, origin/master)
    Author: Radon Rosborough <radon.neon@gmail.com>
    Date:   Sat May 5 15:32:38 2018 -0700

        Add Magit to versions lockfile

    commit e648627f2ae954f244ae2c4140dd356b3b9d39d9
    Author: Radon Rosborough <radon.neon@gmail.com>
    Date:   Sat May 5 12:34:11 2018 -0700

        Add Magit

    commit b0a04734b553d32ff0065b0ac09c18f01517807c
    Author: Radon Rosborough <radon.neon@gmail.com>
    Date:   Fri May 4 21:58:31 2018 -0700

        Note operating system

To pull any commits that were added to the upstream repository since
you cloned it, run:

    $ git pull origin master

Whereas to push any commits that you have added up to your fork, run:

    $ git push <remote-name> master

If you have multiple computers with this repository, then you might
also want to fetch new commits from your fork:

    $ git pull <remote-name> master

The meaning of `master` here is the name of the *branch* you are
manipulating. In a simple repository like this, you will only have one
branch. (The name of the branch that's created by default is
`master`.)

## Basic Emacs navigation

The keybinding notation is abbreviated for convenience. `C-b` means
control-b, `M-f` means option-f (but Emacs calls option as "meta",
hence the `M`). `S-<left>` means shift-leftarrow. There are also
multi-key sequences. So `C-c M-J` would mean control-c, followed by
meta-shift-j. The enter key is denoted by `RET`.

Quit:

    C-x C-c

Cancel command or key sequence in progress:

    C-g

Run an arbitrary command, like `eval-buffer`:

    M-x eval-buffer RET

Edit a file in the current window (type in the path name, and press
`RET`; note that you can press `TAB` to get help from Emacs with
this):

    C-x C-f

Save changes to the current file:

    C-x C-s

Undo:

    C-/

Split the current window into two, top and bottom:

    C-x 2

Split the current window into two, left and right:

    C-x 3

Switch between windows (but you can also just click using the mouse):

    C-x o

Close the current window:

    C-x 0

Switch the current window to another file that you already opened
(usually faster than using `C-x C-f`; `TAB` also works here, and if
you just press `RET` without entering anything, Emacs usually picks a
nice default):

    C-x b

## Getting help

Find out what happens when you press a keybinding:

    C-h k

Show the last few keybindings you triggered, and what commands they
ran:

    C-h l

Find the documentation for a function or variable:

    C-h o

## Editing Emacs configuration

Whenever Emacs starts up, it loads the code in
`~/.emacs.d/init.el`. If you change the configuration, then restarting
Emacs will cause the changes to take effect. However, you don't need
to restart to get the changes.

You can evaluate the previous s-expression with `C-x C-e`. This means
that Emacs looks backward from the cursor to find the last closing
parenthesis. Then it pairs that parenthesis with its matching opening
parenthesis, and evaluates the code enclosed.

You can evaluate the current top-level s-expression with
`C-M-x`. (Top-level means not enclosed by another pair of
parentheses.)

You can evaluate the whole file with `M-x eval-buffer`. Finally, if
you press `M-:` then Emacs will prompt you for a piece of code to
evaluate.

## General Org usage

Go to your todo list (as if you had used `C-x C-f` to find it):

    C-c t

Cycle visibility of current subtree:

    TAB

Cycle visibility of entire tree:

    S-TAB

Mark a TODO item as DONE (this actually cycles through all the defined
TODO item states):

    C-c C-t

Move forward through TODO states:

    S-<right>

Move backward through TODO states:

    S-<left>

Insert a new header:

    M-RET

Increase indentation level:

    M-<right>

Decrease indentation level:

    M-<left>

Drag current heading upwards:

    M-<up>

Drag current heading downwards:

    M-<down>

Go to previous item:

    C-<up>

Go to next item:

    C-<down>

Go to previous heading:

    M-{

Go to next heading:

    M-}

Set deadline:

    C-c C-d

Set scheduled date:

    C-c C-s

Remove deadline:

    C-u C-c C-d

Remove scheduled date:

    C-u C-c C-s

When cursor is on a deadline or scheduled date, reschedule to one day
earlier:

    S-<left>

And one day later:

    S-<right>

Edit tags for the current heading:

    C-c C-q

Set the priority for a TODO item:

    C-c ,

## Org capture usage

Capture a TODO item:

    C-c c

Insert completed item into todo list:

    C-c C-c

Cancel capture:

    C-c C-k

## Org clock usage

Clock in on the current task:

    C-c C-x C-i

Clock out:

    C-c C-x C-o

Clock in to the last task you clocked out of:

    C-c C-x C-x

Clock into a task, selecting from a list of recent ones:

    C-u C-c C-x C-i

Jump to the currently or previously clocked task in your todo list:

    C-c C-x C-j

## Org agenda usage

Open the agenda view:

    C-c a

Mark a TODO item as DONE:

    t

Change the deadline or scheduled date to one day earlier:

    S-<left>

Change the deadline or scheduled date to one day later:

    S-<right>

Clock in:

    I

Clock out:

    O

Save the file:

    s

## Magit usage

Instead of using Git from the command line, you can use it through
Magit inside of Emacs. Open the status buffer (similar to `git
status`):

    C-x g

Stage the file at the cursor (similar to `git add`):

    s

Unstage (similar to `git reset`):

    u

Commit (similar to `git commit`):

    c c

Submit your commit message:

    C-c C-c

Cancel your commit:

    C-c C-k

Pull changes from a remote (similar to `git pull`):

    F e

Push changes to a remote (similar to `git push`):

    P e

Add a remote:

    M a

Look at the URLs of remotes:

    M C

[commit-messages]: https://chris.beams.io/posts/git-commit/
[homebrew]: https://brew.sh/
[student-pack]: https://education.github.com/pack
