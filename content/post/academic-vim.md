---
date: 2017-03-22T00:05:36-04:00
draft: false
title: Using Vim for Academic Writing
tags:
  - writing
  - vim
  - configuration
  - practices
categories:
  - Practices
  - Writing
  - Tooling
---

Writing is one of the most important activities for an academic. An academic's job is to create and
disseminate knowledge, and this cannot be done without effective writing. Thus, it is important to 
pick the right tools for the job. These tools can be roughly split into the categories of 
[editor](#vim), [typesetting language](#pandoc-and-latex), and [support tools](#plugins). In this 
post, I explain my academic writing workflow using Neovim, Pandoc and LaTeX, and a curated set of 
plugins to assist with various writing tasks.

<!--more-->

# Writing with Neovim {#vim}

In the programmer community, the "Emacs or Vim" debate is an unending flame war. I have no intent of 
entering this debate[^vim]. You should use whatever editor works for you; Vim (specifically 
[Neovim](https://neovim.io)) works for me. However --- particularly in computer science --- there 
seems to be a bias toward Emacs, so I'll spend a bit of time explaining my preference.

I will note that for more formal academic writing (mostly in fields other than computer science), 
many people like something more along the lines of Microsoft Word or a specialized LaTeX editor. 
There are good reasons to use these programs, but using Vim[^editors] is often the better choice. 
Work in computer science and other STEM fields involves writing in a blend of formats --- coding in 
various languages, writing formal papers with formulae, graphics, and ordinary text, and (for some 
of us) making detailed research notes for planning. Given this diversity of formats, there's a 
benefit to using the same editor for them all, if that editor can adequately support them. 
Fortunately, Vim, Emacs, and most other common programming editors have excellent support for all of
the formats an academic will regularly need to use.

Assuming that you buy my argument for using a programming editor instead of Word, I still need to 
argue for my choice of Vim in particular. It is my experience that while Vim has a much higher 
learning curve than Emacs, Sublime Text, etc., the investment of time in learning Vim's modal 
editing language pays dividends. Other editors provide their own means of rapidly changing and 
navigating text; I find Vim's language of modifiers and movement operators to be the most expressive
and flexible to use. It's worth noting that most text editors have decent plugins for emulating 
Vim[^spacemacs]; if you already use another editor and want to try Vim's editing paradigm, these can
be a good option.

Finally, I favor Vim over Emacs for its plugin ecosystem, ease of use in the terminal[^terminal], 
and speed. Vim's customization language (Vimscript) is admittedly horrible compared to Emacs Lisp, 
but Neovim fixes some of this deficiency through its remote plugin API. Given that Neovim also adds 
an asynchronous job API[^vim8], cleans up the Vim source code, and offers greater future 
maintainability with no regressions from Vim's functionality, it is the best choice of Vim-family 
editors.

[^vim]: Though Vim is definitely better[^jokes].
[^jokes]: Kidding, kidding. Any Emacs fans in the audience can put down their parenthetical pitchforks.
[^editors]: Or another general purpose editor.
[^spacemacs]: [Spacemacs](https://spacemacs.org) is a particularly good option.
[^terminal]: Which probably only matters if you are a computer scientist.
[^vim8]: Vim 8 also added an asynchronous job API.

# Pandoc Markdown and LaTeX {#pandoc-and-latex}

foo

# Support Tools {#plugins}

bar
