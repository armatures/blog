This is my blog. It's built with Hakyll. I write Haskell for my day-job, but this blog pre-dates that work. Let me know if you notice anything I could improve on!

Build this project, a static site builder, with `stack build`

Once it's built, run it to build your site with `stack exec site build`

These commands are aliased in `package.json`, so they can be run with ~yarn build`, for example. Also `yarn deploy` and `yarn deploy --prod` will deploy it.

`stack.yaml` specifies which versions of your dependencies will resolve, and `my-site.cabal` is the output of compilation of `package.yaml`.
If you want different versions of dependencies, you can change the versions specified in `stack.yaml`, including potentially updating the resolver version. If you want to add different libraries as dependencies, you can change the list `build-depends` in your cabal file.

I tried setting up hpack, with a `package.yaml` file, but I just wasted a bunch of time and couln't figure out how to list a dependeny on PandocFilterGraphviz.
