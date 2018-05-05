# Miscellaneous cheatsheet

This document has a number of tips and tricks relevant to Emacs usage.

## Command line

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

Install Emacs (on macOS):

    $ brew install emacs --devel --with-cocoa

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

Then you can copy the application from there, and it will be runnable
from the Applications folder:

    $ cp -R /usr/local/Cellar/emacs/26.1-rc1/Emacs.app /Applications

Start Emacs:

    $ emacs

Start Emacs inside the terminal (some features may not work, but some
people prefer it this way):

    $ emacs -nw

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

Edit a file in the current window (type in the path name, and press
`RET`; note that you can press `TAB` to get help from Emacs with
this):

    C-x C-f

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

## General Org usage

Go to your todo list (as if you had used `C-x C-f` to find it):

    C-c t

Cycle visibility of current subtree:

    TAB

Cycle visibility of entire tree:

    S-TAB

Mark a TODO item as DONE:

    C-c t

Insert a new header:

    M-RET

Increase indentation level:

    M-<right>

Decrease indentation level:

    M-<left>

Go to previous item:

    C-<up>

Go to next item:

    C-<down>

Set deadline:

    C-c C-d

Set scheduled date:

    C-c C-s

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
