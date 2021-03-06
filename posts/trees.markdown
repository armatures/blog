---
title: Decision Trees as a Teaching Tool
date: "December 19, 2019"
tags: economics, diagrams
---

Recently I encountered the idea of [fast-and-frugal trees](https://en.wikipedia.org/wiki/Fast-and-frugal_trees) through [an Econtalk conversation with Gerd Gigerenzer](https://www.econtalk.org/gerd-gigerenzer-on-gut-feelings/). I have long been interested in graphical representations of information and I want to explore applications of this idea.
I would usually draw trees by hand, or with illustration software, but Justus Wilhelm already implemented a pandoc filter to interpret graphviz in a Hakyll site, so this post will use that.
<label for="sn-thanks" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-thanks" class="margin-toggle">
  <span class="sidenote">
I discovered this capability through Justus Wilhelm's great post [Hakyll on Netlify](https://www.justus.pw/posts/2019-09-01-hakyll-on-netlify.html).
I did a little poking around in the [repo for that blog](https://github.com/justuswilhelm/personal-website), which is open-source, to figure out the details.
I copied things over to my site's repo, but had some trouble getting diagram generation to work. I [made a few edits](https://github.com/armatures/blog/commit/4fb45913d29a29e92c1cab35d98754f4dfa5de79), and now things are running successfully for me locally.
</span>

## Diagrams with Pandoc

I wanted to put together an example of a fast-and-frugal tree to explore both the idea and the tooling. 
I recently learned Matt Eklund's board game [Pax Transhumanity](https://ionsmg.com/products/pax-transhumanity), and thought it might be a good candidate for such a tree: it’s got lots of interacting parts that aren’t very familiar to new players, and I've been teaching it to lots of friends recently.

Here's a first take on a fast-and-frugal tree to teach a new player very basic strategy:
<label for="sn-strategy" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-strategy" class="margin-toggle">
  <span class="sidenote">
  A player needs to choose what idea cards are most desirable for them, and I could make another tree just to track this. For the time being, I'll probably give them this early-game ranking of idea impacts:
  a company in the first-world > a company in your hidden sphere > a bonus agent
  </span>

```graphviz
    strict digraph {
          rankdir=LR

          isViable [label="Are any ideas viable for you?"]

          viable [label="Have you syndicated a viable idea?"]
          syndicated [label="Commercialize a viable idea."]
          notSyndicated [label="Syndicate a viable idea."]

          notViable [label="Are there any desirable idea cards?"]
          desirable [label="research another card to get patents matching the desired card's 2 disciplines"]
          notDesirable [label="research a card in the most-empty column to bring in new ideas"]

  isViable -> notViable [color=red]
  notViable -> desirable [color=chartreuse2]
  notViable -> notDesirable [color=red]

  isViable -> viable [color=chartreuse2]
  viable -> notSyndicated [color=red]
  viable -> syndicated [color=chartreuse2]
  }
```
It's left things very abstract, and communicating to a new player the idea of what cards are "desirable," even if they know that the next step for them is probably to pursue such a card.
I will try introducing this to someone unfamiliar with the game, and see how it goes.

While we're at it, I should kick the tires on another diagramming tool: [mscgen](http://www.mcternan.me.uk/mscgen/) 
I'm not sure how well these map to domains outside of software (this is where I've encountered these before), but here's a Message Sequence Chart for a couple options from the research action:
```msc
    msc {
      hscale = "2";

      a [label="Player"],b [label="Market"],c [label="Human Progress Splay"];

      ---  [ label = "if no opponent has syndicated researched card" ];
      a=>b [ label = "Research" ];
      b>>a [ label = "Tuck as top Think Tank card"];
      b=>b [ label = "refill" ];
      |||;
      
      ---  [ label = "if you don't want or can't take card as Think Tank" ];
      a=>b [ label = "Research" ];
      b>>a [ label = "2 disciplines from card"];
      a>>c [ label = "install agents from finance board as matching patents"];
      b=>b [ label = "refill" ];
    }
```

I like MSCs a lot for diagramming the interactions between a user, their browser, and a server or some other architecture, though I'm not sure if it works as well here.
Compared to a tree, it really emphasizes the interactions you can have with different parts of a system over time, and I think this is well suited to questions like "how do I add cards to the splay, again?" or "how do I refresh cards in the market?"

This sort of diagram might be able to track, using arrows of different colors or shapes, deposits/withdrawals from different stores of value _over time_. Assigning time to the Y-axis gives the potential for a little more order than some other diagram types
<label for="sn-frank" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-frank" class="margin-toggle">
  <span class="sidenote">
At the risk of repeating myself, MSCs emphasize the flow of time, compared to other diagrams.
FrAnkX0r88 on Boardgamegeek created [another type](https://www.boardgamegeek.com/filepage/191327/lifecycle-pax-transhumanity) of [UML diagram](https://en.wikipedia.org/wiki/Unified_Modeling_Language#Diagrams) to document some of the interactions between systems in this game.
</span>
, but any of these representations might be too abstract for many players. I imagine tools like this are best suited to designers of complex systems, but I'd like to explore the application of these types of diagrams further.

Pax Transhumanity is a good test case for some of this sort of exploration, because it's got lots of interacting parts that aren't very familiar to new players.
I pared the interactions down to those that happen in a single action to make it easier to parse, but you've always got to consider whether you're giving someone enough information to be useful without overwhelming them.


