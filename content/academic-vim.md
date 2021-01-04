+++
date = 2017-03-22
draft = false
title = "Using Vim for Academic Writing"
author = "Wil Thomason"
bibliography = "/home/wil/gdrive/notes/references.bib"
link-citations = true
reference-section-title = "References"

[taxonomies]
tags = ["neovim", "academia", "practices", "tools"]
+++

Writing is arguably the most important activity for an academic. An academic's job is to create and
disseminate knowledge, and this cannot be done without effective writing. Thus, it is important to 
pick the right tools for the job. These tools are split into the categories of [editor](#vim), 
[typesetting language](#pandoc-and-latex), and [support tools](#plugins). In this post, I explain my 
academic writing workflow with Neovim, Pandoc and LaTeX, and a curated set of plugins for
writing tasks.

If you already use my combination of Neovim, Pandoc, and LaTeX, you'll probably want to skip ahead 
to [the last section](#plugins).

<!-- more -->

# Writing with Neovim {#vim}

In the programmer community, the "Emacs or Vim" debate is an unending flame war. I have no intent of 
entering this debate[^vim]. Use whichever editor works for you; Vim (specifically 
[Neovim](https://neovim.io)) works for me. However --- particularly in computer science --- there 
seems to be a bias toward Emacs, so I'll explain my preference.

For formal academic writing (especially in fields outside of computer science), tools like Microsoft 
Word or a specialized LaTeX editor are popular. There are good reasons to use these programs, but 
using Vim[^editors] is often the better choice. Work in computer science and other STEM fields 
involves writing in a blend of formats --- coding in various languages, writing formal papers with 
formulae, graphics, and ordinary text, and (for some of us) making detailed research notes for 
planning. Given this diversity of formats, there's a benefit to using the same editor for them all, 
if that editor can adequately support them. Fortunately, Vim, Emacs, and most other common 
programming editors have excellent support for all the formats an academic will regularly need to 
use.

Assuming you buy my argument for using a programming editor instead of Word, I still need to argue 
for my choice of Vim in particular. In my experience, though Vim has a much higher learning curve 
than Emacs, Sublime, etc., the investment of time in learning Vim's modal editing language pays 
dividends. Other editors provide means of rapidly changing and navigating text; I find Vim's 
language of modifiers and movement operators the most expressive and flexible. It's worth noting 
that most text editors have decent plugins for emulating Vim[^spacemacs]; if you already use another 
editor and want to try Vim's editing paradigm, these can
be a good option.

Finally, I favor Vim over Emacs for its plugin ecosystem, ease of use in the terminal[^terminal], 
and speed. Vim's customization language (Vim script) is admittedly horrible compared to Emacs Lisp, 
but Neovim fixes some of this deficiency through its remote plugin API. Given that Neovim also adds 
an asynchronous job API[^vim8], cleans up the Vim source code, and offers greater future 
maintainability with no regressions from Vim's functionality, it is the best choice of Vim-family 
editors.

# Pandoc Markdown and LaTeX {#pandoc-and-latex}

Regardless of the editor you choose, it is important to give thought to the right language[^lang] to 
use for writing. There are many options --- popular ones include plain text, Markdown of various 
flavors, Wiki format, Emacs' ``org-mode``, and LaTeX --- but I think [Pandoc](https://pandoc.org) 
and [LaTeX](https://www.latex-project.org/) form the best pairing for academic writing. I'll give 
some background on both and explain why I have this preference.

Pandoc is a wonderful tool for converting text written in one of many formats to an even larger set 
of output formats. It takes as input Markdown, Emacs Org-Mode syntax, Microsoft Word files, and 
several other formats[^formats], and outputs PDFs, LaTeX, HTML, and others. In typical use, you 
write in Pandoc Markdown --- ordinary Markdown[^markdown] extended with some useful features 
(including footnotes, tables, definition lists, embedded math, etc.) --- and output whatever format 
you need. Pandoc is great for academic writing because it allows you to easily make 
professional-looking documents with figures, tables, automatically-processed citations, and math. 
Further, because Pandoc can convert Markdown to many output formats, it's trivial to write your 
content once and publish it in several forms (e.g. in a PDF for submission to a journal or 
conference, in HTML for publication on a website, etc.). Plus, Markdown is so easy and natural to 
write that you can complete documents very quickly. In fact, I wrote this entire post in Markdown.

With all that said, Markdown does have some shortcomings: Though the "default" is usually very nice, 
it does not give you enough expressive power to customize every aspect of your document's appearance 
and typesetting. For that, you need something like LaTeX. To quote the project's "About" 
page[^latex-cite], "LaTeX...is a document preparation system for high-quality typesetting." It is 
already heavily used in several areas of academia (CS, mathematics, and physics in particular), but 
is suitable for any form of academic writing. LaTeX's "best known" strength is its phenomenal 
typesetting of math symbols, but it also provides rich facilities for controlling the layout and 
appearance of your document. Using LaTeX has an associated learning curve, but there are more good 
learning resources[^sharelatex] now than ever. Moreover, mastering LaTeX is well
worth the work - the most beautiful academic documents I've seen use LaTeX for formatting.

So, why are Pandoc and LaTeX **together** a great setup for academic writing? Pandoc is very easy to
write, and very easy to use to get an attractive result --- including math, figures, and the like. 
However, it isn't easy to tweak appearance in Pandoc Markdown --- but it is in LaTeX. Fortunately, 
Pandoc can convert from Markdown *straight to* LaTeX. This functionality presents an attractive 
workflow: You can start a project writing in Markdown. Use it for your notes, your planning, and 
your early paper drafts. When you reach the stage of a project where the content is largely done and
it is time to focus on tweaking the appearance and making everything "just so", you can run Pandoc 
on your Markdown file to output LaTeX source and continue editing with greater control.

Thus, by starting your writing in Markdown and ending with LaTeX, you get a nice balance of ease of 
use for the 80% of the work that doesn't require tremendous power, and all the typesetting power you
could want for the remaining 20%.

# Support Tools {#plugins}

The final piece pulling together the elements of my academic writing setup is a curated set of 
writing plugins for Neovim. These fall loosely into three categories: [language 
plugins](#lang-plugins), [focus plugins](#focus-plugins), and [editing plugins](#edit-plugins). I do
use many plugins other than these; you can see the full set in my [dotfiles 
repo](https://github.com/wbthomason/.dotfiles).

# Language Plugins {#lang-plugins}

Language plugins are plugins which add support to Neovim for new languages. While Neovim offers 
basic syntax highlighting support for many languages out of the box, there's a lot more that these 
plugins have to offer.

I use the following plugins for Pandoc Markdown:

- [vim-pandoc](https://github.com/vim-pandoc/vim-pandoc): Adds several integrations between Vim and 
  Pandoc 
  - the ones I rely on are auto-completion of bibliography entries and correct handling of
  hard and soft line wraps.
- [vim-pandoc-syntax](https://github.com/vim-pandoc/vim-pandoc-syntax): Provides syntax highlighting
  for Pandoc Markdown files. In particular, it also integrates with Vim's `conceal` feature, which 
  makes certain elements of Pandoc's formatting syntax render in more visually appealing ways.
- [vim-pandoc-after](https://github.com/vim-pandoc/vim-pandoc-after): Adds integrations with 
  [unite.vim](https://github.com/Shougo/unite.vim)[^unite-bibtex] and 
  [UltiSnips](https://github.com/SirVer/ultisnips).

I also use[^self-promo] a plugin I wrote, [buildit.nvim](https://github.com/wbthomason/buildit.nvim)
to compile my Markdown into PDF form using a Makefile. While `vim-pandoc` can asynchronously run the
Pandoc program, I find it easier to type the options I want to pass just once into a Makefile and 
know that my collaborators can also easily build the text without having to type the exact same 
invocation.

For LaTeX, I am a big fan of [vimtex](https://github.com/lervag/vimtex). `vimtex` offers 
auto-compilation on every save, completion of citation and file names, text objects for LaTeX 
environments, commands, etc., better LaTeX syntax highlighting, and new mappings for working with 
LaTeX.

# Focus Plugins {#focus-plugins}

A focus plugin is a plugin designed to help you focus on what you're writing. While working on a 
large document, it can be easy to become distracted by sections other than the one you're currently 
writing, indicators of length or other metrics in the editor, and other extraneous information. The 
two focus plugins I use help to eliminate these distractions and let you pay attention to the 
writing at hand.

The first of these focus plugins is [goyo.vim](https://github.com/junegunn/goyo.vim). `goyo` 
essentially hides all the unnecessary parts of the editor while you're writing: Line numbers, status 
displays, etc. It focuses the text in the center of the screen. While this functionality sounds 
simple, it's surprisingly nice for writing.

My second focus plugin is [limelight.vim](https://github.com/junegunn/limelight.vim). `limelight` 
works very well with `goyo`, as it brightens the color of the current paragraph of text, and dims 
everything else. In other words, everything but what you need to be focused on fades to the 
background.

# Editing Plugins {#edit-plugins}

The final set of plugins I use for writing falls into the category of editing plugins. An editing 
plugin is one designed to help make your writing better --- things like grammar checks, word usage, 
and style issues.

For the first of these issues, I use [vim-grammarous](https://github.com/rhysd/vim-grammarous). 
`vim-grammarous`, as the name implies, checks your writing for grammar errors using 
[LanguageTool](https://www.languagetool.org/). It can also use the suggested fixes from 
`LanguageTool` to automatically edit your writing. It's as simple as that, but very useful.

[vim-ditto](https://github.com/dbmrq/vim-ditto) helps to avoid using the same words ad nauseam in 
your writing[^ditto-pandoc]. If it detects that you've used the same word too many times too close 
to one another (ignoring, of course, very common words), it will highlight the overused term in red.
This plugin is another great example of a simple piece of functionality that can drastically improve
your writing.

Finally, for more advanced checks of usage and style, I like 
[vim-wordy](https://github.com/reedes/vim-wordy). While I'm a relatively new user of `vim-wordy` 
myself, and thus still learning all it can do, it is a very powerful plugin for writers. It can 
check for things like weak and lazy words, redundant or problematic usage, weasel words, the passive
voice, and much, much more. I typically run `vim-wordy` once I've finished my first draft, to 
highlight areas in need of special editing attention.

# Notes

[^vim]: Though Vim is definitely better[^jokes].

[^editors]: Or another general purpose editor.

[^spacemacs]: [Spacemacs](https://spacemacs.org) is a particularly good option.

[^terminal]: Which probably only matters if you are a computer scientist.

[^vim8]: Vim 8 also added an asynchronous job API.

[^lang]: Programming language or format, not natural language.

[^formats]: All of which are listed on [Pandoc's website](https://pandoc.org).

[^markdown]: A very simple language for structuring text, commonly used by Github and many other websites. See [here](https://daringfireball.net/projects/markdown/) for more.

[^latex-cite]: Found here: https://www.latex-project.org/about/

[^sharelatex]: For example, [these](https://www.sharelatex.com/learn).

[^unite-bibtex]: If you're an Unite or Denite user, then [unite-bibtex](https://github.com/msprev/unite-bibtex) is a good thing to have.

[^self-promo]: Warning: Shameless self-promotion here.

[^ditto-pandoc]: For instance, it's going crazy on the word "Pandoc" in this piece.

[^jokes]: Kidding, kidding. Any Emacs fans in the audience can put down their parenthetical pitchforks.
