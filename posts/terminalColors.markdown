---
title: "RGB Colors in your terminal"
date: "March 5, 2022"
tags: haskell posix sh terminal rgb colors
---

I have never gotten along well with bash. It is unintuitive, poorly documented, and doesn't have many guarantees you would expect with a modern language. It, like JavaScript, has the "killer feature "of being portable, so it's equally ubiquitous. That's the only reason to use it, if you ask me! Macs ship with `zsh` as the default shell, now, and it's an improvement far getting things done in day to day usage, but that doesn't mean I want to write an application in it.

I'm sorry to start a post about coloring terminal output by bashing shell languages 😉🤦, but I guess I felt the need to explain why I've written haskell instead of bash functions. A short explanation of what's going on, and then the code.

Beyond the usual set of visible characters, terminals have representations for unprintable "control" characters. To get terminal output to show up in a different color, we input an "escape" character followed by a few arguments separated by semicolons. While this makes sense to me in the abstract, nobody looking at the code would call it "intuitive," and there's no way I'm going to remember the sequence of characters representing this. They're very arbitrary. I'm sure there are `sh` functions that make this more ergonomic, but... like most things that have lasted a long time, it's so encumbered by historical baggage and the need for backwards compatibility that there's no chance of making it actually pleasant to work with. As opposed to the code below which is compatible with the 1998 Haskell language standard. _So_ modern!:

``` haskell
#!/usr/bin/env stack
-- stack runghc --resolver lts-18.26

main =
  putStrLn (rgbForeground 14 100 10 "green foreground on its own line")
    >> putStrLn "usual-colored text"
    >> putStrLn
      ( rgbBackground 144 30 40 "red background "
          ++ (rgbBackground 203 208 70 "sharing a line")
      )
    >> putStrLn
      ( rgbBackground 144 30 40 $
          "red background & "
            ++ (rgbForeground 3 8 70 "nested foreground")
      )

-- | each `Int` should be in the range 0-255
rgbForeground :: Int -> Int -> Int -> String -> String
rgbForeground r' g' b' s =
  let r = show r'; g = show g'; b = show b'
   in concat ["\ESC[38;2;", r, ";", g, ";", b, "m", s, "\ESC[0m"]

-- | each `Int` should be in the range 0-255
rgbBackground :: Int -> Int -> Int -> String -> String
rgbBackground r g b s =
  "\ESC[48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m" ++ s ++ "\ESC[0m"
```

To summarize, your terminal will interpret semicolon-delimited decimal characters between`\ESC[` and `m` specially. `0` as the first character resets the output to its default, and setting the first 2 arguments to `38` and `2` set an expectation for 3 more arguments representing red, green, and blue channels to represent the foreground color. There is a ton more to know about this, and you can read many pages of docs on the subject... but really, nobody wants that. This post is a wrapper around 2 one-line functions that give an intuitive (obvious?) and decently-typed interface to a pretty outrageously obfuscated spec.
