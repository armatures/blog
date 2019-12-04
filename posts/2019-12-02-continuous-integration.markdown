---
title: Continuous Integration and Deployment
---

"How should I host my statically-generated website?" I thought to myself, considering several available options.
  * S3 buckets: gwern uses S3 buckets, I could too
  * netlify: I currently have a site on Pivotal's [hosted Cloud Foundry](https://run.pivotal.io/) and another on [netlify](https://www.netlify.com/), so adding many more is going to get messy. Cloud Foundry is a great option if you want to run backend services, but for a statically-generated blog, I don't need any of that. I just want to give my site directory to someone and be done with it. It looks like netlify should work for this use case, but I'm not sure how best to run the build before deploying it.

  Initially, this hosting option didn't feel like a great fit. Netlify's main sell is its convenient hosting of a git repo, and in this case, things are a little messier. I loved my first experience hosting with them, but my build step was javascript-based<label for="sn-elm" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-elm" class="margin-toggle">
  <span class="sidenote">  Don't worry, it was [Elm](https://elm-lang.org/), but you can fetch Elm with [this npm package.](https://www.npmjs.com/package/elm)</span>, and now I wanted to convince them to run Haskell.

The first question to ask here is "Can I install Haskell through npm?" but that seemed a little too far-out for me. Netlify seems to only expect [dependencies in JS, Ruby, or Python](https://docs.netlify.com/configure-builds/manage-dependencies/#dependency-cache), and they don't have many build image options (none with Haskell), so I'm not sure they'd be willing to fetch me the massive binaries for GHC regardless of how I requested them. Netlify does have an option to deploy without using their CI, and that's likely my best option. 
  I recently found [a post](https://www.justus.pw/posts/2019-09-01-hakyll-on-netlify.html) by Justus Perlwitz documenting his approach to hosting a hakyll site on netlify, and he didn't use their CI: this confirms my initial thinking that it would be a bad fit, but gives me a great path forward elsewhere: [CircleCI](https://circleci.com/).
