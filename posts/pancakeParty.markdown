---
title: "Pancake Party"
date: "February 22, 2020"
tags: economics, coordination problems, inadequate equilibria
---
I do not often eat pancakes. Though pancakes are delicious, they are an uncommon enough food for me that I do not own syrup or pancake-making ingredients. If I want to eat pancakes, I will probably coordinate to do so with a friend. Let's model a cooperative pancake breakfast with a simple game.

<div class="stamp-container"> <div class="centered" style="width:200px"><img alt="pancakes: batter fried in a pan" src="../static/pancakes.png"/> <caption> pancakes: fried batter slabs </caption> </div></div>

## A Pancake Game

You and I will coordinate to bring pancake necessities to our meeting (you can bring syrup, I will bring the cakes). Payouts are 1 if both players choose to bring party supplies and zero in any other case
<label for="sn-payoff-matrix" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-payoff-matrix" class="margin-toggle">
  <span class="sidenote">
    This diagram is a _payoff matrix._ The pairs of numbers in the cells of the table represent the payouts to the two players of the game, and the column and row of each cell represent the choices made by the players.
    </span>:

### Free Participation
<caption>
Payouts given cost for players to participate is zero
<table class="border" style="margin:0;padding:0;">
<tbody><tr>
<th class="border diagonal-line"><div class="right">player B</div><div class="left">player A</div>
</th>
<th>B brings <br>supplies
</th>
<th>B defects
</th></tr>
<tr>
<th>A brings <br>supplies
</th>
<td class="border diagonal-line"><div class="right">1</div><div class="left">1</div>
</td>
<td class="border diagonal-line"><div class="right">0</div><div class="left">0</div>
</td></tr>
<tr>
<th>A defects
</th>
<td class="border diagonal-line"><div class="right">0</div><div class="left">0</div>
</td>
<td class="border diagonal-line"><div class="right">0</div><div class="left">0</div>
</td></tr></tbody></table>
</caption>

This gives a weakly dominant<label for="sn-payoff-matrix" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-payoff-matrix" class="margin-toggle">
  <span class="sidenote">
    "Weakly dominant" means that choosing this strategy always gives at least as good an outcome as choosing an alternative strategy, regardless of what the other player does. There is also at least one action the other player can take for which it's better than the alternative strategy. If the strategy is _always_ better, it's said to be "strictly dominant."
    </span> strategy of playing C to both players. If this model is accurate, both players will play C, and get a payout.
What happens if there is a cost to changing strategy, and both players currently have chosen D? Maybe the players don't have pancake ingredients on hand, and would have to procure them, or maybe it is difficult to transport eggs around in your backpack on the bus. The players can choose to take some risk on making the pancake party happen, or take the lazy, safe bet of doing nothing.
We can think of this game with a new payout matrix:


### Costly Participation
<caption>
Payouts given a cost of participation for players
<table class="border" style="margin:0;padding:0;">
<tbody><tr>
<th class="border diagonal-line"><div class="right">player B</div><div class="left">player A</div>
</th>
<th>B brings <br>supplies
</th>
<th>B defects
</th></tr>
<tr>
<th>A brings <br>supplies
</th>
<td class="border diagonal-line"><div class="right">1</div><div class="left">1</div>
</td>
<td class="border diagonal-line"><div class="right">0</div><div class="left">-1</div>
</td></tr>
<tr>
<th>A defects
</th>
<td class="border diagonal-line"><div class="right">-1</div><div class="left">0</div>
</td>
<td class="border diagonal-line"><div class="right">0</div><div class="left">0</div>
</td></tr></tbody></table>
</caption>

This is a classic prisoner's dilemma. There is a cost to changing strategies (or a risk), but if both players change strategies, there is a payout, and hopefully precedent for games in the future. In this case, paying the cost to change strategies is not obviously good. The assurance of someone else changing strategies as well would give a payout, but you're unlikely to get there without explicit coordination. 

<div class="stamp-container"> <div class="centered" style="width:200px"><img alt="a pancake pan" src="../static/pancakePan.png"/> <caption> the pan: a key pancake party component </caption> </div></div>

## A Pancake Party

In the more general case, I'm imagining many people, each of whom will bring a different ingredient to the pancake party. The initial proposal is to invite a big group of guests, and assign each of them a required ingredient in the invitation. It would be a catastrophe if someone with a critical ingredient missed the party. In all likelihood, as we increase the number of players that need to show up, and give them other games or parties to attend, it's unlikely we'll be able to make this party happen at all. Players might look for a more certain good time elsewhere, if they're not certain they can rely on the other invitees.

Modeling these strategy-changing costs like this feels like it decently captures the core of lots of coordination problems. In this case, what could we do to advance the interests of the pancake party?

- Commitment: Instead of hoping people will attend, and assigning everyone ingredients in the invitation, the party can get the commitment of members ahead of time. If people are trustworthy, and commit to attending, each member can rest easier knowing that the other attendees at least plan to attend with their ingredients.

- Commitment with Enforcement: can we punish anyone that doesn't come to our party?

- Coordination ahead of time: can everyone drop their ingredients off at the venue the day before the breakfast? Then all the attendees can see the ingredients on the webcam, and know that pancakes are happening.

- Meet at the grocer's: everyone bring your ingredients to the grocery store! If Steve fails to show up, we'll be able to pay for the missing ingredient.

- Money: send me money and I'll buy all the ingredients. We don't get to use our own contributions, but we'll definitely have everything.

- Sponsor: the party has a sponsor (I'm looking at you, church fundraising breakfast) that can front the ingredients. Attendees that couldn't trust their fellow pancake-folk now have the assurance of a capable institution bringing all the ingredients, so each of them is certain the party is on, and can attend without fear that their efforts will be wasted. If the church kitchen has all the ingredients, just in case someone drops out, then complete failure is averted if  one attendee fails to show.

- Restaurant: how about... we just pay for the pancakes, and the kitchen, and the labor to create them, and the space to eat them in, and give up on this party thing.

Maybe there our other solutions to coordinating these would be pancake party participants. I'm hoping to generalize from this about other coordination problems in a future post but I think this is a fun concrete case to consider. Even if the above list of party options is silly, are there analogous choices that make sense for a different, more important coordination problem?

<div class="stamp-container"> <div class="centered" style="width:200px"><img alt="pancakes in color" src="../static/pancakeInkColor.png"/> <caption> maybe pancakes are not critically important after all </caption> </div></div>
