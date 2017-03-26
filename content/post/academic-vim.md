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
post, I explain my academic writing workflow with Neovim, Pandoc and LaTeX, and a curated set of 
plugins to assist with various writing tasks.

<!--more-->

If you already use my combination of Neovim, Pandoc, and LaTeX, you'll probably want to skip ahead 
to [the last section](#plugins).

## Writing with Neovim {#vim}

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
argue for my choice of Vim in particular. In my experience, while Vim has a much higher learning 
curve than Emacs, Sublime, etc., the investment of time in learning Vim's modal editing language 
pays dividends. Other editors provide their own means of rapidly changing and navigating text; I 
find Vim's language of modifiers and movement operators to be the most expressive
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

## Pandoc Markdown and LaTeX {#pandoc-and-latex}

Regardless of the editor you choose, it is important to give thought to the right language[^lang] to 
use for writing. There are many options --- popular ones include plain text, Markdown of various 
flavors, Wiki format, Emacs' ``org-mode``, and LaTeX --- but I think [Pandoc](https://pandoc.org) 
and [LaTeX](https://www.latex-project.org/) form the best pairing for academic writing. I'll give 
some background on both and explain why I have this preference.

Pandoc is a wonderful tool for converting text written in one of a surprisingly large number of 
formats to an even larger number of output formats. It can take in Markdown, Emacs Org-Mode syntax, 
Microsoft Word files, and several other formats[^formats], and output PDFs, LaTeX, HTML, and much 
more. In typical use, you write in Pandoc Markdown --- ordinary Markdown[^markdown] extended with 
some useful features (including footnotes, tables, defintion lists, embedded math, etc.) --- and 
output whatever format you need. Pandoc is great for academic writing because it allows you to 
easily make professional-looking documents with figures, tables, automatically-processed citations, 
and math. Further, because Pandoc can convert to so many output formats, it's trivial to write your 
content once and publish it in several forms (e.g. in a PDF for submission to a journal or 
conference, in HTML for publication on a website, etc.). Plus, Markdown is so easy and natural to 
write that you can complete documents very quickly. In fact, I wrote this entire post in Markdown.

With all that said, Markdown does have some shortcomings: Although the "default" is usually very 
nice, it does not give you enough expressive power to customize every aspect of your document's 
appearance and typesetting. For that, you need something like LaTeX. To quote the project's "About" 
page[^latex-cite], "LaTeX...is a document preparation system for high-quality typesetting." It is 
already heavily used in several areas of academia (CS, mathematics, and physics in particular), but 
is suitable for any form of academic writing. LaTeX's "best known" strength is its phenomenal 
typesetting of math symbols, but it also provides rich facilities for controlling the layout and 
appearance of your document. Although there can be a learning curve associated with using LaTeX, 
there are more good learning resources[^sharelatex] now than ever. Moreover, mastering LaTeX is well
worth the work - the most beautiful academic documents I've seen use LaTeX for their formatting.

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

[^lang]: Programming language or format, not natural language.
[^formats]: All of which are listed on [`pandoc`'s website'](https://pandoc.org).
[^markdown]: A very simple language for structuring text, commonly used by Github and many other websites. See [here](https://daringfireball.net/projects/markdown/) for more.
[^latex-cite]: Found here: https://www.latex-project.org/about/
[^sharelatex]: For example, [these](https://www.sharelatex.com/learn).

## Support Tools {#plugins}

The final piece pulling together the elements of my academic writing setup is a curated set of 
writing plugins for Neovim.
