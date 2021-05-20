Build this project, a static site builder, with `stack build`

Once it's built, run it to build your site with (I think) `stack exec site build`

`stack.yaml` specifies which versions of your dependencies will resolve, `package.yaml` specifies what your dependencies are, and `blog.cabal` is the output of compilation of `package.yaml`. You shouldn't have to manually work with `blog.cabal` at all.
If you introduce new dependencies, update `package.yaml`. If you want different versions of them, you can change the versions specified in `stack.yaml`, including potentially updating the resolver version.
