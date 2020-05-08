---
title: Bipartisan Space
date: "May 6, 2020"
tags: politics, diagrams, software, architecture
---

I'm planning to build a new web app. First I'll explain the idea of it, and then explore the architectural decisions involved.

## The Three Languages of Politics 

Recently, I was listening to [Russ Roberts interview Arnold Kling](https://www.econtalk.org/arnold-kling-on-the-three-languages-of-politics-revisited/). Arnold Kling wrote a book titled _The Three Languages of Politics_, in which he describes a framework to categorize political belief. In his first edition of this book, political beliefs are mapped into a space defined by three axes<label for="sn-axes" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-axes" class="margin-toggle">
  <span class="sidenote">
the three original axes are: the liberal axis, which views society as a tension between the exploited and their exploiters; the conservative axis, which views society as intention between civilization and barbarism; and the libertarian axis, which views society as a tug-of-war between freedom being impinged on by coercion. </span>. Political communication between people with different beliefs often fails, with people talking past each other. Kling models this by imagining conversation partners moving on axes orthogonal to one another. People frame conversations about these topics in different ways and don't influence each other, because they're looking at different goal posts<label for="sn-goalposts" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-goalposts" class="margin-toggle">
  <span class="sidenote">For example, an argument informed by a conservative perspective may deal with topics that a liberal audience views as irrelevant, failing to influence them. The issues different partisans judge as worth considering are different, depending on their affiliations.</span>. This is a compelling idea, and his first guest appearance on EconTalk made an impression on me. In his second appearance, he talks about the changes made to the book in its second and third editions, highlighting a new dividing aspect of our politics: populism and nationalism.

During Kling's second guest appearance on Econtalk, Russ Roberts said something else that stood out to me:

> I did go through a phase--it was very short-lived--where I thought it might be a good thing for people to do activities that their in-group doesn't normally do. This would be Republicans going to yoga classes, and Democrats going to NASCAR. But Lilliana Mason<label for="sn-mason" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-mason" class="margin-toggle">
  <span class="sidenote">Lilliana Mason is associate professor of Government and Politics at the University of Maryland, College Park, and author of _Uncivil Agreement: How Politics Became Our Identity_.</span> worried that NASCAR was a dangerous place for African-Americans and I thought that was an interesting example of some of the problems we're talking about. That, even the idea of going to a non-political event where you know that people are a certain way culturally or politically, could be fraught with danger, is I think symptomatic of really, a huge cultural problem. It's part of the reason, I think, people have trouble with patriotism today. It's like, 'Whose country are you talking about, mine or theirs? I like my version of patriotism, but theirs is dangerous.' I think that's really scary."

Is he right? It doesn't take much imagination to imagine places that feel unsafe for black people. A NASCAR race might be such a place.

I started thinking about ways to explore this idea, and landed on a website where people could go to share bipartisan space safely. The website would graph visitors on Kling's political axes, describing their political diversity. The site wouldn't allow users to interact—no one would consider it a "social network," and it might be closer to social commentary or thinkpiece than serving people's active demands. In any case, What would it look like to build?

## Architecture
The frontend needs are easy enough to define: a user would have a few sliders to set their values in the three dimensions, and with these set, the user would see the axes with all the visitors on them. Other users' dots would appear and disappear as they come and go (this is the most interesting technical requirement).

A collection of user dots implies a datastore of some sort. Usually, I'd put a server in front of the database and make REST requests over HTTP to it. In the interests of expanding my horizons, I'll try something a little less familiar. GraphQL, maybe, instead of REST endpoints... But that still leaves the difficult requirement for the page to update as people come and go. I've consumed websockets on the frontend, and they're more complicated than I would have expected. My last employer used a paid service to establish websocket connections, which seems odd for an open protocol I'd expect is fairly common. So that's puzzling, and might be interesting to look into. I've also heard good things about firebase, and it specializes in doing things very quickly and at scale. This is the option I plan to go with.

So, assuming I set up a Cloud Firestore account, what then? The frontend could talk to it directly, without a server. This is attractive in its simplicity, but raises questions of permissions. I don't want _any_ client to be able to edit all the files on the server, I just want a client to be able to notify the DB when it arrives or leaves. This is not out-of-the-box functionality, nor is it something we can do with anonymous clients. From looking over the docs, It's not clear what sort of identifying data is available in a request when it is received, but an IP address seems like the best option that doesn't demand a user to register an account with the site. An IP address gives no guaranteed uniqueness, and a user could change their IP address and confuse the site. Why not just have people register accounts and call it good?

The likeliest outcome for this site is that it never receives noticeable traffic. Site registration may scare off new users, and I want to avoid that if possible. Using IP addresses to identify users will give the site marginally worse data than having everyone register, but the site is likely to get more users that way, and I expect the data to be fairly reliable so long as nobody is actively trying to hack the site. I'll write either a controller endpoint or a serverless function that can view the client's IP address and store that alongside their political coordinates. If they reload the page, I'd like the page to remember their coordinates and show them the axes with everyone's coordinates on them. Because leaving the page will send a request to the server declaring that they've left, the server won't have their coordinates displayed once they leave. But because they didn't make an express decision to reselect their preferences, they shouldn't have to choose them again. This is probably most easily done using localstorage: when they load the page, the app can check if they have previously selected political coordinates, and either show the coordinate-selection form or the axes, depending on the result.

```msc
    msc {
      hscale = "2";

      a [label="Client"],b [label="LocalStorage"],c [label="Server"],d [label="Database"];

      a box d  [ label = "Case 1: this is the user's first time submitting their political coordinates", textcolour="orange", linecolour="orange" ];
      a=>b [ label = "fetch saved coordinates" ];
      b>>a [ label = "no coordinates found", textcolour="orange"];
      a=>a [ label = "show coordinate form", textcolour="orange", linecolour="orange" ];
      a=>c [ label = "submit filled form", textcolour="orange" ];
      c=>d [ label = "insert (IP Address, coordinates)" ];
      a=>b [ label = "save coordinates" ];
      a=>d [ label = "fetch all user coordinates" ];
      d>>a [ label = "list all user coordinates" ];
      a=>a [ label = "show summary page" ];
      ...;
      |||;
      a=>a [ label = "user is leaving page; onbeforeunload event" ];
      a=>c [ label = "user is leaving" ];
      c=>d [ label = "remove (IP Address, coordinates)" ];
      |||;
      
      a box d  [ label = "Case 2: the user has previously submitted their coordinates", textcolour="orange", linecolour="orange" ];
      a=>b [ label = "fetch saved coordinates" ];
      b>>a [ label = "return saved coordinates", textcolour="orange"];
      a=>c [ label = "submit saved form data", textcolour="orange" ];
      c=>d [ label = "insert (IP Address, coordinates)" ];
      a=>d [ label = "fetch all user coordinates" ];
      d>>a [ label = "list all user coordinates" ];
      a=>a [ label = "show summary page" ];
      ...;
      a=>a [ label = "user is leaving page; onbeforeunload event" ];
      a=>c [ label = "user is leaving" ];
      c=>d [ label = "remove (IP Address, coordinates)" ];
    }
```

This [MSC](https://en.wikipedia.org/wiki/Message_sequence_chart) shows how the behavior of the app changes when saved coordinates are found in localstorage as compared to the first time the user visits the site.

The notable thing here is that the database receives something like "(IP Address, Coordinates)", and will overwrite the previous entry for that IP Address if someone resubmits the form. If there are multiple users in a private network, they may overwrite one anothers' data, and I'm not sure how to prevent that. It seems to me they either get generous permissions, and can write too much, or we define users by their IP Address and in some cases they may overwrite one another.

Let me know if you have any better ideas!
