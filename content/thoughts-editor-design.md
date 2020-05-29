+++
title = "Thoughts on Text Editor Design"
date = 2018-01-10T12:31:53-05:00
draft = true
+++

This is a messy collection of thoughts about what a proper, modern text editor should be alongside
some thoughts/planning for the implementation of such an editor. I pulled it straight from my
personal notes, so it's not cleaned up and was originally intended for my personal use. I'm putting
it here so that I can more easily get some external opinions on it.

<!-- more -->

Note that by "Vim" I usually actually mean "Neovim" or "both Vim and Neovim".

## Features/Properties

These are the properties I think are important for any modern text editor, listed in no particular order.

### Must-have

- **Speed**: The best thing about Vim is how light and fast it feels. It should start roughly as fast as
Vim, and editing should feel instant.
- **Extensibility/Customizability:** Emacs is great because almost every facet of it can be
customized and tweaked. Vim plugins feel like ugly hacks in comparison, and a lot of Vim feels stuck
in stone --- you can't change arbitrary parts. Ideally, the language for customization should be the
same as the language for implementing the editor to allow for this sort of ultimate customizability.
- **Terminal-first:** Vim has a great terminal UX and many meh GUI UX's[^mehgui]. Emacs has a meh
  terminal UX[^mehmacs]
and a great GUI UX. It's nice to have a good GUI, but for my use terminal comes first.
- **Async-capable:** While it's not a great idea to implement a threading/async scheduling system
*inside* the editor (as AFAIK all of Vim, Neovim, and Emacs have done), having async computation
is a must. Ideally this should be derived from the language runtime used for extensibility.
- **Completions/pop-ups:** There needs to be some way to call a function and display the results in
a pop up, such as for autocompletion. There should also be a default (but overridable) autocompleter.
- **Regex search:** It should be trivial and fast to search through one or more buffers with a
regex, as well as to perform find/replace operations and edits.
- **Fast syntax highlighting:** 'Nuff said. Emacs does better than Vim here.
- **Minimal redraws:** 'Nuff said. More of a means of achieving speed than anything else.
- **Good defaults:** Neovim does a decent job of this, but the default settings should be sane and
represent the most common use case.
- **Multiple buffers/Windows/Layouts:** It should be easy to have split windows, multiple buffers in
a ring, tabs with different layouts, etc.
- **A left and right gutter, tabline, and status line:** Not all of these have to display at all
times, but they should all be possible to display.
- **A good default (but overridable) plugin manager:** The user shouldn't have to install anything
in order to download, install, and use plugins/extensions, but they should be able to replace the
plugin manager with their own solution if they choose.
- **Theming:** It should be possible to theme the various editor components.
- **Buffer markup:** Things like line numbers, indentation, whitespace, etc. should be able to be
quickly displayed.
- **Efficient and extensible movement/commands:** Vim does this better than anyone. The editor
commands are a composable language allowing for rapid and repeatable movement and editing.
- **Symbol support:** The editor should play nice with ligatures and other "fancy symbols", a la
Vim's `conceal` feature. 
- **Formatting support:** Bold, italic, underline, colored versions of these, etc.
- **Internal command execution:** Like Vim (and Emacs).
- **Unicode support:** Duh.
- **Keymapping support:** Any key should be mappable to any function/command in the editor.
- **Server mode/Remote UI:** Neovim and Emacs are best in class here. Basically, the core editor
should be a server, and multiple clients should be able to connect to it. However, unlike Emacs, the
editor should be able to run multiple servers at once.
- **Simple configuration language:** Even if plugins can be written in a "real" programming
  language, configuring the editor should be done through a better suited language --- something
  like YAML, TOML, or JSON[^bothlangs].

[^mehgui]: In my opinion, at least. I know some people love GVim/MacVim/etc., but I've never found them as nice to use as terminal Vim.

[^mehmacs]: Again, just my opinion, and relative to Vim.

[^bothlangs]: I still need to figure out how to ergonomically have two different languages for configuration/customizability like this. Maybe I should look at how Sublime does this?

### Niceties

- **LSP integration:** Adoption of an editor is more likely when the ecosystem of plugins and
features is strong. The LSP provides a way to get state-of-the-art completion and other language
features for free with some degree of future-proofing. Having this support baked in makes it faster,
which is good for such a core piece of functionality.
- **Multi-language extensions:** Not everyone wants to have to learn a new language to customize
their editor, so it is useful to have a mechanism for writing (maybe limited) extensions in
arbitrary languages. A remote API like Neovim's is a good way to do this.
- **Good handling of large files**: Emacs does better than Vim here. It can load very large files
from disk and edit with no/minimal latency compared to the lag Vim sometimes sees.
- **A fuzzy-finder/filter:** Helm, FZF, Unite/Denite, etc. are among the most important/popular
class of plugins for their respective editors. The utility of being able to quickly filter down a
list of {files, buffers, settings, commands, whatever arbitrary data, etc.} is not easily
overstated. This should probably be implemented as an extension, but making sure that good support
for such an extension exists is important.
- **A file browser:** Something a little more than `netrw` should be included in the editor.
- **Multiple cursor support:** Like Sublime.
- **External command execution:** It should be possible to run a command and see its output/write it to the
buffer. Like Vim/presumably also Emacs.
- **Something like the quickfix list**: To allow for easy grouped interaction with output.
- **Reconfiguration without the need to reload the editor:** Config files should be able to be read
and applied without a total editor reload.
- **Delimiter matching:** This is fundamental enough to coding that it should probably be built in.
- **Hard and soft line wraps:** People like both of these.
- **Text re-flowing:** Like `gq` in Vim/presumably an equivalent in Emacs.

## Implementation Planning

All of these notes are useless if they don't help with an actual editor, so I'd like to implement my
own[^funtimes]. This section has some high-level preliminary planning on how to go about doing this.

[^funtimes]: This also just seems fun.

### Language

The language used to implement the editor should be fast, so probably something compiled. However,
to make extensibility easy, it would be ideal if the language also had an embeddable interpreter (to
avoid the necessity of compiling plugins and re-linking), or some other non-remote API mechanism of
evaluating code. Finally, because I'm lazy, I want the language to have good TUI/GUI packages
already in its ecosystem.

OCaml seems like a good choice for this. It's highly performant and has an embeddable toplevel
interpreter. Of course, OCaml is known and used by a relatively small population, which may make
native plugin development less likely to take off.

[Rasa](https://github.com/ChrisPenner/rasa) uses Haskell and is notable for its extreme modularity.
It might be worthwhile to look into this either as an option for extension or just to copy the
method of script extension.

Something really fast like Rust would be good if I can figure out how to allow total
customizability/extensibility.

A Lisp would be cool, but maybe not as fast. In cursory inspection, I also haven't found a dialect
with solid libraries for all the things I want.

### Data Structure

Ropes seem to be the best choice overall, though gap buffers are a common choice for simplicity.
Some parts of the Internet like piece tables, but these don't seem to be chosen by most other modern
text editors, and have some shortcomings.
