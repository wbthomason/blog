---
layout: post
title: "ai in insufficient depth"
date: 2015-12-10 00:43:57
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
bibliography: nooope.bib
---

As a first-year PhD student at [Cornell][dept-site], I'm required
to prove my competency in the most fundamental areas of CS. Per departmental
standards, this proof is offered either through taking (and passing) a graduate
class in the area, or demonstrating preexisting knowledge of the relevant topics
by sitting for the final exam for the corresponding undergraduate course[^1].

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
<!-- more -->

The below topics are largely drawn from the "course summary" created for
Cornell's [CS 4700][ai-course], which itself is largely drawn from
[@russell_artificial_2010]. In general, @russell_artificial_2010 is an excellent
resource, and is highly recommended to anyone who wishes to go in greater depth
on these subjects.

# Agents and Environments

## Agent Types

## Environment Types

## Rationality

# Search

## Properties of Search Algorithms

## Uninformed Search

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

### Minmax Trees

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

#### Temporal-Difference Learning

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

### Architecture

#### Layers

#### Activation Functions

### Perceptron Learning

### Backpropagation Learning

#### Gradient Descent

### Simulating Boolean Gates


# References

[^1]: Typically, the exam is preferable only if the area has little relation to
  one's work.

[dept-site]: https://www.cs.cornell.edu
[ai-winter]: https://en.wikipedia.org/wiki/AI_winter
[ai-course]: https://www.cs.cornell.edu/courses/cs4700/2015fa/
