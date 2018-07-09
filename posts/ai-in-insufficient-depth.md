---
title: "ai in insufficient depth"
date: 2015-12-08T02:05:21-05:00
tags:
  - AI
  - tutorials
  - review
  - high-level
  - CS
  - link collections
categories:
  - Tutorials
  - CS
  - AI/ML
  - Overview
bibliography: citations.bib
link-citations: true
draft: true
---

As a first-year PhD student at [Cornell][dept-site], I'm required
to prove my competency in the most fundamental areas of CS. Per departmental
standards, this proof is offered either through taking (and passing) a graduate
class in the area, or demonstrating preexisting knowledge of the relevant topics
by sitting for the final exam for the corresponding undergraduate course[^exam].

I'm taking this second option for the AI area requirement; although my work is
certainly pertinent to modern AI, the course tends to focus on older topics
which are [less relevant to modern work][ai-winter], and it is generally
recommended that most students take the exam.

Given this, and given that I tend to learn best by writing about and teaching
a subject, I intend this post to serve as a reasonable study guide for AI, taken
broadly. Going into **all** of AI in any real depth would require more writing
than I intend to do; however, this depth should prove unnecessary for the
purpose of this post. It will provide an overview of the topics covered by the
course, and a host of links to further resources. It is not a one-stop reference
for AI; nor is it intended to be. It is a high-level rundown of the key points
of topics, and little more.

With that said, let's get started.
[ai-winter]: https://en.wikipedia.org/wiki/AI_winter
[dept-site]: https://www.cs.cornell.edu
[^exam]: Typically, the exam is preferable only if the area has little relation to one's work.

<!--more-->

The below topics are largely drawn from the "course summary" created for
Cornell's [CS 4700][ai-course], which itself is largely drawn from
[@russell_artificial_2010]. In general, [@russell_artificial_2010] is an excellent
resource, and is highly recommended to anyone who wishes to go in greater depth
on these subjects[^dated]. I cite it heavily throughout this guide.

# Agents and Environments

Classical AI tends to define its problems in terms of **agents** and
**environments**. Broadly, the **environment** is essentially what you'd expect
--- the world surrounding an agent, which we can think of as the problem space.
An **agent** is anything that has "read/write" access to the environment ---
that is, it can both perceive (read) and alter (write) facets of the
environment[^pg34]. We categorize both agents and environments based upon their
features.

## Agent Types

The primary types of agents in classical AI are as follows:

Simple Reflex Agents
: As the name implies, *simple reflex agents* are both simple and purely reactive.
  At a given timestep, they choose an action based only upon their current
  observation of the environment, with no incorporation of history. Ignoring
  past experience in this manner quite obviously limits their capabilities; as
  stated on p. 49 of [@russell_artificial_2010], simple reflex agents only work if
  the environment is **fully observable**[^coming_soon].

Model-Based Reflex Agents
: The first and most obvious means of adapting a reflex agent to a partially
  observable[^coming_sooner] environment is to give it a means of internally treating the
  environment as more observable than it is. *Model-based reflex agents* do this
  by storing an internal state which combines their previous observations about
  the environment with their current observation according to a set of rules
  encoding how the agent thinks the environment "works". This set of rules is
  called the model. This doesn't make the environment truly fully observable,
  but provides an approximation of a fully observable environment.

The rest of this will be fleshed out ASAP; however, filling in the sections on
Logic and Learning is currently more pressing.

## Environment Types

## Rationality

# Search

Many problems in AI can be characterized in terms of an abstract[^concrete]
**search space** --- a set of states, possible inputs, etc. --- which an agent
tries to efficiently and effectively explore. As such, search and search
algorithms are a large component of classical AI[^robots].

[^concrete]: Or, occasionally, concrete.
[^robots]: As well as some areas of modern robotics.

## Properties of Search Algorithms

There are certain properties of search algorithms which prove useful for
categorizing them. Most of these are obvious to a student of computer science:
We can categorize algorithms by their complexity in space and time, their means
of exploration, etc. However, a few are less commonly seen outside of AI. First,
there's the difference between **graph search**, which is a class of search
algorithms that never visits a node twice, and **tree search**, which allows for
revisiting of nodes. We can also talk about algorithms as being **complete**
(meaning they'll always find a solution if one exists) or **optimal** (meaning
that they find a solution as quickly as is possible). Finally, given how large
search/state spaces tend to be in AI[^billions], and given that search trees can
grow exponentially as states are added to the set to explore, it can be
important to discuss the **effective branching factor**, or the average number
of successor states which must be added when an arbitrary node in a search
problem is explored [@_branching_2014].

[^billions]: Think on the order of billions of states for common problems.

## Uninformed Search

**Uninformed search** algorithms are what most computer science students are
accustomed to from their other classes; they use no special information about
the particular problem represented by their search space to guide their
exploration. They tend to be the simplest algorithms, but also potentially the
least efficient.

### DFS

### BFS

### Uniform Cost

### Bidirectional Search

## Informed Search

### Greedy (Best-first)

### A*

#### Heuristics

## Local Search

### Hill Search

### Simulated Annealing

### Beam Search

### Genetic Algorithms

## Adversarial Search

### Minimax Trees

### $\alpha$-$\beta$ Pruning

### Exhaustive Search

# Logic and Knowledge

## Logical Agents

## Representing Knowledge

## Inferring Knowledge

### Inference Rules

### Resolution Refutation

## Soundness and Completeness

## Derivation and Entailment

## Propositional Logic

### Syntax and Semantics

### Logical Rules

### Translating to/from Propositional Logic

## First-Order Logic

### Syntax and Semantics

### Composition

### Quantification

### Unification

# Learning

## Reinforcement Learning

### Learning Utilities

#### Sampling

#### Adaptive Dynamic Programming

From the book: 

> ADP learns a model and a reward function from observations and then uses value
> or policy iteration to obtain the utilities of an optimal policy. ADP makes
> optimal use of the local constraints on utilities of states imposed through
> the neighborhood structure of the environment.

#### Temporal-Difference Learning

From the book:

> Temporal-difference methods update utility estimates to match those of
> successor states. They can be viewed as simple approximations of the ADP
> approach that can learn without requiring a transition model. Using a learned
> model to generate pseudoexperiences can, however, result in faster learning.

#### Q Temporal-Difference Learning

### Selecting Actions

### Dealing with Large State Spaces

## Machine Learning

### Supervised and Unsupervised Learning

### Classification and Regression

### Hypothesis Spaces

### Overfitting and Underfitting

### Cross-Validation

### Performance Measures

## Decision Trees

### Learning Algorithm

### Pruning

## Neural Networks

The key things to know about neural networks are their inputs and outputs, their
general structure, how we train them, and where they are stronger and weaker. It
is important to note that this section is **not** even remotely close to up to
date with modern neural network research; it is scoped mostly to older-style
(circa 2010) neural networks, as is necessary for the competency exam.

A neural network takes as its inputs some $n$ many values, which are usually
continuous. These inputs correspond to some set of features or descriptors of
some data upon which we wish to compute a function --- usually,
a classification[^universal] of the data into some group. If we have an input
$X=(x_1,x_2,\ldots,x_t)$, then a neural network which works with $X$ will take
$t$ inputs.

The output of a neural network can technically be any value, but for our
purposes can be thought of a classification. Generally, if we have $k$ many
classes, we will have $k$ outputs from the corresponding neural network (though
this is not always the case). A significant counterexample to this is the set of
problems where we are computing a binary classification; here, we will more
likely have a single output which holds a certain value for one class, and
a different value for the other. The exact number and output encoding of the
outputs of a network can vary wildly depending on the needs of the network (i.e.
the problem being solved); the main point is that, for our purposes, the output
of a neural network is a classification.

Now, let's talk about what comprises a neural network.

### Architecture

Fundamentally, a neural network is made up of **nodes** (or neurons) organized
in **layers** and connected with directed, weighted edges. In the following
sections, we'll describe what these parts are and how they fit together.

A node is a vertex in the graph representing the network. It operates quite
simply: Given its inputs (the incoming edges to the graph), it computes
a weighted sum of their values (the weights come from the edges), and uses this
sum as the input to its activation function, the resulting value of which it
then outputs on its outgoing edges.

We generally parameterize a neural network by three things: how it looks (the
layers and their interconnections), how it acts (the activation function
chosen), and how it learns (the weight update method chosen). 

#### Layers and Interconnections

When we look at a neural network, we usually look at a directed graph: As
previously stated, the nodes are vertices in the graph, and the weighted,
directed edges connecting them determine how values flow through the network.
These connections determine the characteristics of the network: for our
purposes, we usually talk about **feed-forward** networks, in which nodes are
organized into **layers** that only pass values forward (toward the output
nodes), and not between nodes in the same layer or backward. However, other
connection schemes are possible[^recurrent]. For instance, **recurrent neural networks**
connect backward in the network, allowing them to support a sort of "memory" for
values. Networks with intralayer connections do exist, but are much less common
(so far as I'm aware).

We call a feed-forward network with only one layer of
neurons between the inputs and outputs a **perceptron**; this was one of the
earliest forms of neural network. It's important to note that perceptrons are
really just linear classifiers; if a function is not linearly separable[^xor],
a perceptron *cannot* represent it.

Fixing this inadequacy requires the addition of more layers between the input
and output, which we call **hidden layers**. Having these layers essentially
allows a network to compute an alternate representation of the input, which can
correspond to a higher-level feature.

#### Activation Functions

The activation function determines what a node outputs. In practice, a number of
different activation functions are used; for our purpose, either a logistic
curve or step function is generally used. There's a threshold assigned to the
node, and if the value of the activation function is above this threshold, the
node "activates"; otherwise, it does not[^continuous]. Nodes can also have an associated
**bias**, which is an additive factor modifying the value of the inputs to
a node, and thereby affecting the value of the activation function of their sum.

### Perceptron Learning

In a perceptron, weights are updated using the **perceptron learning rule**,
which is $$w_i\leftarrow w_i+\alpha(y-h_w(x))\times x_i$$
for a weight $w_i$, a learning rate $\alpha$, a desired output $y$, an actual output $h_w(x)$, and an
input $x_i$. Essentially, by iteratively applying this rule, we're minimizing
the error (the difference between the expected and actual outputs) by changing
the weights in the appropriate direction.

### Backpropagation Learning

For multilayer networks, we more commonly use back-propagation learning. The
main concept of back-propagation is that we're pushing error in the output
backward through the layers, attributing changes to weights based off of how
much the error changes with respect to the weight. In essence, we're following
the gradient of the error function --- we call this **gradient descent** -- to
change the weights so as to find a minimum of the function.

[^universal]: It's important to note, though, that neural networks are universal --- they can compute any arbitrary function.
[^recurrent]: And, in fact, quite successful --- recurrent neural networks are a very active area of modern neural net research.
[^xor]: The canonical example is the xor function.
[^continuous]: I believe that continuous activation is possible/used, but I don't think we need to know about it at all.

# References

[^dated]: It's a bit dated --- the latest version was published in 2010, and a lot has changed in AI (and ML in particular) since --- but it remains an excellent introduction to classical AI.
[^pg34]: See [@russell_artificial_2010], pp. 34
[^coming_soon]: We'll learn what this means in just a moment.
[^coming_sooner]: Again, we'll be defining this soon.

[ai-course]: https://www.cs.cornell.edu/courses/cs4700/2015fa/