# Emacs Configuration Files

This is my current Emacs setup. I've pieced it together out of a lot of other wonderful files shared by mush smarter folks than me.

I use Emacs mostly to write and debug Clojure, using the Cider REPL and several modes like Paredit, to keep things tidy.

It also has some modes for Markdown and Org-Mode, which I am experimenting with.

## How it works
Everything is kept in an org file called "configuration.org", a highly commented and structured human-readable file with snippets of Elisp. The actual init.el file parses this and generates a "configuration.el" file (using org-babel-load-file) from the snippets, a clean and minimal init script.

## Organization

I have the settings broken into 9 categories.  Defaults, Keybindings, Packages, Load Path, Shell, Navigation, UI, Editing, and Languages. 

Sub-sections are individual functions, with a code-block. Most of them have comments explaining the purpose of a given setting.

## Sources

Inside you will find code shamelessly stolen from:

- Clojure for the Brave and True (most of his Clojure-y bits)
- Harry R Schwartz (extensively copied [his configuration.org file](https://github.com/hrs/dotfiles/blob/master/emacs/.emacs.d/configuration.org)  and implementation)
- Stack Overflow answers
- My dad's Emacs config and favorite packages

##Dead code

I'm working to minimize this. However when you copy large portions of several other people's code, cruft accumulates. Over time this will get pruned back to what I'm actually using. For now, some of the files are not actually in use.
