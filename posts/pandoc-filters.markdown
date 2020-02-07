---
title: Custom Pandoc Filters in Haskell
date: "December 13, 2019"
tags: haskell, pandoc, blogging
---

## The Expressiveness/Verbosity Tradeoff

HTML is a programming language used to display nearly everything you view on the internet.
<label for="sn-html" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-html" class="margin-toggle">
  <span class="sidenote">
    At the highest layer, everything ends up being wrapped in HTML. There are other types of media online, using different encodings, though most of these are not intended to be human-readable. People sometimes manipulate SVG manually, and I'm sure there are other exceptions, but HTML is the primary language your browser interprets when deciding what to show on screen.
  </span>
Your browser knows how to read it and convert it to geometry for you to look at. It is very expressive―people have figured out how to display nearly anything with it―but it can be very verbose. This is a common tradeoff when choosing digital tools: do you need a very general tool that has not been optimized for your specific use case, or can you choose something a little more tailored to what you're doing?

For this blog, I decided HTML is too verbose and unspecialized for my needs. Instead, it is currently written in [Markdown](https://www.markdownguide.org/basic-syntax), a language designed for this sort of composition. It's very convenient that someone else has already defined a standard for documents in general, and using a standard means things will be well supported in the future. But using too many things too thoroughly defined by other people really defeats the purpose of building one's own website: I am interested in developing tools to produce things digitally with more freedom than I have with plaintext, or even with Markdown. How can I get all the expressiveness of HTML without all of its verbosity?

## A Tool for Tool-making

Creating _arbitrary_ rules for translating text into other formats will allow for the creation of interesting shorthand or visually rich text treatments: when authoring, I am interested in using expressive idioms of communication, and not repeating myself, and these tools should allow for automation of repetitive writing tasks.

[Pandoc](https://github.com/jgm/pandoc) is a Haskell tool for converting between markup formats. It is very extensible, so though most of this blog is standard Markdown, I can add custom Pandoc behavior to optimize for common use-cases.
<label for="sn-macfarlane" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-macfarlane" class="margin-toggle">
  <span class="sidenote">
The backstory of Pandoc may be as interesting as the tool itself. It was written (and is currently maintained) by [John MacFarlane](https://johnmacfarlane.net/), a Berkeley Philosophy Professor with a penchant for Haskell. Parsers are something of a "killer app" for Haskell, and MacFarlane's take on document translation appealed to practical hackers all over. It turned out to be more long-lived and maintainable than its competitors, and may be one of the most well-known Haskell packages around, despite its author specializing in an entirely different field. Thanks, John MacFarlane!</span>
Or so I hoped. I set up this blog with the optimistic notion that such customization should be simple for me. [Other](https://www.gwern.net/) [people](https://www.justus.pw/) seem to have great success with this set of tools. I'm sure their success has not been without its struggles: I thought it might be useful to document mine.

## Pandoc Filters

Pandoc can be extended through filters. These programs can access the intermediate representation Pandoc uses internally as it translates between formats (in my case, Markdown and HTML), and make edits and introduce new rules. Sidenotes are currently implemented with raw HTML, and I currently paste in some sidenote boilerplate to add them to my posts. This process is disruptive to my composition and noisy to read while editing. I would like to write something like `[sn]("example-class")` in my document, and then outside of my paragraph write the contents of the note. With this as my initial goal, I set out to write my first Pandoc Filter in Haskell.

I found [slides from a talk](https://johnmacfarlane.net/BayHac2014/) given by John MacFarlane<label for="sn-talk" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-talk" class="margin-toggle">
  <span class="sidenote">
  The talk is 5½ years old, but it's probably still relevant, right?
  </span>, with [an associated repo](https://github.com/jgm/BayHac2014) containing some exercises for the unininitiated. I headed over to the [Pandoc documentation on filters](https://pandoc.org/filters.html) and got to work. After looking over the docs, I loaded up [the example](https://pandoc.org/filters.html#a-simple-example) written there. It's introduced as "A simple example," but took an hour of debugging to get working.
<label for="sn-runhaskell" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-runhaskell" class="margin-toggle">
  <span class="sidenote">
Aside from the type errors when trying to compile the filter, running it gave me an entirely different set of problems: when trying to run `pandoc -f markdown -t HTML5 1.md --filter behead.hs`, I kept getting a message of `Could not find executable runhaskell`. I tried `stack build runhaskell`, only to get `Unknown package: runhaskell`. It turned out wrapping everything in `stack exec` fixed this problem for me: `stack exec -- pandoc -f markdown -t HTML5 1.md --filter behead.hs` works great.
  </span>
  I got myriad errors about type signatures; the API must have changed for writing these filters. In the end, I ended up deleting much of the example. The definition of the main function: `main = interact (writeDoc . walk behead . readDoc)` relied on Pandoc's internals accepting `String`s, and the intermediate functions that wrapped the important parts of the filter, `writeDoc` and `readDoc`, were written expecting Markdown specifically. `toJSONFilter` is much more general than that, so once I discovered that, I wrapped `behead` in that and deleted everything else:

```haskell
-- behead.hs
import Text.Pandoc
import Text.Pandoc.JSON

behead :: Block -> Block
behead (Header n _ xs) | n >= 2 = Para [Emph xs]
behead x = x

main :: IO ()
main = toJSONFilter behead
```

My first working Pandoc filter! This felt like a substantial win: it's surprisingly harder than you think to find examples of viable Pandoc filters written in Haskell. I expect the interface changed recently, so this post might be helpful. I'll also open a pull request to update the filter docs.

My to-do list is getting away from me, and I won't have time to return to this development for a week at least. I am sad to see a lack of syntax highlighting in the haskell code in this post, and I'd like to get back to this to fix it... but in the spirit of publishing things where possible, I'll leave this here.

