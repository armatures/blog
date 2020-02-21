---
date: "February 21, 2020"
title: "Marty Cagan's Recommended Opportunity Assessment"
---

If only to practice using the tool, I'm going to do a formal "opportunity assessment" as described by Marty Cagan in his book _Inspired_. My friend and former colleague Brandon Fortune recommended the book to me, and I've been trying to internalize some of its advice. I am generally more engineer than product manager, but so long as I intend to stay self-employed, I really need to work on the market-oriented side of this equation (my natural state seems to be spending all my time working with code nobody is interested in).


## Opportunity: Boardgame Library Wiki

There are a couple boardgame libraries in the Twin Cities area, but neither of them has their inventory listed anywhere. Currently, if you're interested in trying a game without buying it, you show up at a game library with friends, and depending on what you find, you read the rulebook on the spot, and teach yourself the game. With a reliable-enough list of games in the library, you could plan on playing one of them and learn the rules a little more passively ahead of time. Coordinating availability with friends is difficult, so this time is scarce and needs to be used efficiently.

This seems like a good use case for a wiki. This should not be difficult to do, but I've never created a wiki before, and this is exactly the type of thing I'm prone to underestimate. Sounds like a good opportunity to use Cagan's tool.

1. Exactly what problem will this solve? (value proposition)

I don't know what games might be available without visiting a game library in person.

2. For whom do we solve that problem? (target market)

Boardgame enthusiasts that want to play games they don't own. Boardgame shops that would like to attract these potential customers to their spaces.

3. How big is the opportunity? (market size)

Two (maybe more) shops in the Twin Cities.

4. What alternatives are out there? (competitive landscape)

Dallas Games Marathon maintains [a list](https://www.boardgamegeek.com/collection/user/DGM%20Library) of their games on BoardGameGeek, so this might work for some use cases I'm imagining. This demands the attention of the administrator of a given game library, and free access to edit as a wiki sounds much easier to maintain. That said, the DGM list linked above is an excellent user experience: all the games are linked to an authoritative list cataloging every detail you'd ever want about their games. This amount of detail would demand a lot of wiki-contributors.

5. Why are we best suited to pursue this? (our differentiator)

I don't have any special aptitude for this, though I have used computers before and this sort of information problem seems well-suited to being solved with computers.

6. Why now? (market window)

Nobody seems to have a solution for this problem right now. Wikis are an interesting technology from an economic perspective (I don't know whether they're interesting to administer).

7. How will we get this product to market? (go-to-market strategy)

You'd want buy-in from the business owners hosting the libraries. This would make signage at the library possible. A QR code and web address could be posted next to the game shelves. Visitors to the libraries are the target audience for the wiki, so accessing them via the library most would be critical for this to succeed.

8. How will we measure success/make money from this product? (metrics/revenue strategy)

If people find this valuable, the traffic to the site could create an audience for related innovation. Attention is valuable, and could be monetized through ads.

9. What factors are critical to success? (solution requirements)

- Buy-in from library hosts
- Investment on the part of library visitors (spending time they could spend gaming updating game inventory list on the wiki)
- DGM's solution should be difficult, because it's so good. If it's easy, we should copy them. Asking them how they do it seems like it would be valuable.

10. Given the above, what’s the recommendation? (go or no-go)

This seems like a very small opportunity. If the solution is low-effort, it could be a good candidate problem to solve. Fixing small problems could be good practice to work up to solving big problems.



## Implementation

You've probably noticed the absence of talk about solutions to this problem. That is deliberate, the idea being that if you are well-suited to solve the problem, you shouldn't scrap the opportunity if a specific idea doesn't work out. This _does_ feel lacking to me, though: without evaluating a specific solution, how can we estimate the cost/benefit of the opportunity? In a [blog post about the technique](), Cagan writes "we’re not describing the product here but rather making clear any dependencies or constraints." Cagan reiterates throughout _Inspired_ that the major responsibility of a product manager is to "discover a product that is valuable, usable, and feasible." I'll have to revisit the book to see what further recommendations he has for how to do that once you've chosen an opportunity.

Even though I'm not supposed to think too hard about the implementation of a potential solution, implementation is where I'll be spending a lot of my time, if I choose to pursue the opportunity.
John MacFarlane wrote a wiki engine in Haskell called [gitit](https://github.com/jgm/gitit) that might be interesting to use. To set such a thing up, you'd follow the instructions on the wiki, and then find a host. If introduction to a single local library was valuable, there would be value in introducing it to other local game libraries, and linking other resources to make the listings more valuable.

I _do_ have concrete next steps, if this problem is worth spending more time on. I can definitely commit to doing this assessment more times, but I don't know how much I've learned about this specific opportunity. The assessment was very easy to fill out, so the only opportunities _not_ worth doing this for are probably things that take less than a day or 2.
<label for="sn-concrete" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-concrete" class="margin-toggle">
  <span class="sidenote">  
As I wrote that, I noticed that part of me was trying to get that number as small as possible. Investigating that part of me more closely, I think I am trying to compensate for my natural impatience with this sort of reflection. This sort of attention to process runs counter to my years of experience as a software & business consultant. Heavy, inflexible processes are the bane of progress in the large companies I've worked with, and in my own life, I wonder if I've tried to cut out pointless speculation in some effort to _trim the fat_ and _get lean_ in an effort to _just build something_. I don't know that there's a very conscious justification to this. Do most people experience under-developed processes in their personal lives, and pointlessly-cumbersome processes in organizations they work with? Accountability to other people creates inflexible commitments, so it makes sense that such things might accumulate.
Assessing an opportunity every couple days sounds very frequent. I'll probably make a goal to do this a few more times before I can figure out the types of opportunites it's best suited for.
</span>

Are you interested in hosting a wiki? I would use it, though maybe not very frequently. I'm not sure if I'm interested in hosting one, and regardless of the rest of the assessment, that is the most important part of this decision. After doing a little more research (contacting DGM, and the hosts of local game libraries), I can better tell if this could be valuable. If the idea checks out on that level, I can install a few wiki engines locally and see if those things seem fun. 
