---
title: "hidden markov models in OCaml"
date: 2015-12-08T02:05:21-05:00
draft: true
---

In this thrilling installment of *something bloglike*, we're going to learn
about Hidden Markov Models (more succinctly referred to as HMMs). We'll start
with the math behind the model, and then work through a practical
implementation, which the Magical Wheel of Languages™ says will be in OCaml.
You can find the final implementation [here][crouchingtigerhiddenmarkov].
Everything necessary will be explained from fundamentals, so you shouldn't
need any preexisting knowledge (other than familiarity with OCaml) to get the
gist of the tutorial. That said, a basic grounding in probability may help you
grasp the concepts more easily.

[crouchingtigerhiddenmarkov]: https://github.com/wbthomason/crouchingtiger-hiddenmarkov

<!--more-->


# overview

Let's imagine that you have a problem[^loser]. As a lowly intern at Microoglebook™,
you've been tasked with predicting the sentiment expressed by posts collected
from your users on Facemer+™, the world-famous flagship social media Internet of
Things networking game[^dog]. What's more, due to your boss' healthy case of Wheel
Reinvention Syndrome[^aka], you can't simply call out to an [API][alchemy] or use
a standard NLP[^nlp] [library](corenlp). Nevertheless, you're unfazed. "Easy!",
you think, "This will be a lovely jaunt in the park for a computer scientist of
my abilities.[^cocky]" As such, you spend your summer drinking beer, playing
ping-pong, and generally goofing off. It's not until someone asks you if your
final project presentation is done that you realize: You haven't the faintest
idea how to go about categorizing sentiment. Sure, you think that it could be
modeled as a set of hidden states characterized by probabilistic transitions and
output observations, but it's as though those words fell into your head from the
sky[^narrator] --- they're Greek to you as far as translating them into any sort
of implementation or formalization[^greek]. Late into the night you stare
blankly at your screen. "If only there were a well-established, computationally
efficient, effective, and narratively convenient model applicable to this
problem!", you think to yourself. "If only!"

Well, dear reader/lowly peon/Google's scraper, I have good news for you!

[^loser]: This shouldn't be all that hard to do.
[^dog]: For dogs.
[^aka]: Known in some circles as "stupidity".
[^nlp]: Natural Language Processing. See, not all of these are useless!
[^cocky]: You cocky bastard.
[^narrator]: Or were placed there by a wise and handsome narrator.
[^greek]: Note that you are not fluent in Greek. Δεν έχει ακόμη και αν μπορείτε να διαβάσετε αυτό.

[alchemy]: http://www.alchemyapi.com/api/sentiment-analysis
[corenlp]: https://stanfordnlp.github.io/CoreNLP/


# the math

In the beginning was the Markov Property, and the Markov Property was good. The
Markov property (as a moderate grasp of naming conventions and foreshadowing
should lead you to expect) is really the crux of the entire family of Markov
models, of which there are a fair few.

# the code

# final words

# References
