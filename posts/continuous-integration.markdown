---
title: Continuous Integration and Deployment
date: "December 2, 2019"
tags: blogging
---

"How should I host my statically-generated website?" I thought to myself, considering several available options.
  * S3 buckets: gwern uses S3 buckets, I could too
  * netlify: I currently have a site on Pivotal's [hosted Cloud Foundry](https://run.pivotal.io/) and another on [netlify](https://www.netlify.com/), so adding many more is going to get messy. Cloud Foundry is a great option if you want to run backend services, but for a statically-generated blog, I don't need any of that. I just want to give my site directory to someone and be done with it. It looks like netlify should work for this use case, but I'm not sure how best to run the build before deploying it.

  Initially, this hosting option didn't feel like a great fit. Netlify's main sell is its convenient hosting of a git repo, and in this case, things are a little messier. I loved my first experience hosting with them, but my build step was javascript-based<label for="sn-elm" class="margin-toggle sidenote-number"></label>
  <input type="checkbox" id="sn-elm" class="margin-toggle">
  <span class="sidenote">  Don't worry, it was [Elm](https://elm-lang.org/), but you can fetch Elm with [this npm package.](https://www.npmjs.com/package/elm)</span>, and now I wanted to convince them to run Haskell.

The first question to ask here is "Can I install Haskell through npm?" but that seemed a little too far-out for me. Netlify seems to only expect [dependencies in JS, Ruby, or Python](https://docs.netlify.com/configure-builds/manage-dependencies/#dependency-cache), and they don't have many build image options (none with Haskell), so I'm not sure they'd be willing to fetch me the massive binaries for GHC regardless of how I requested them. Netlify does have an option to deploy without using their CI, and that's likely my best option. 
  I recently found [a post](https://www.justus.pw/posts/2019-09-01-hakyll-on-netlify.html) by Justus Perlwitz documenting his approach to hosting a hakyll site on netlify, and he didn't use their CI: this confirms my initial thinking that it would be a bad fit, but gives me a great path forward elsewhere: [CircleCI](https://circleci.com/).

  I copied the `.circleci/config.yml` file over from [Justus' GitHub repo](https://github.com/justuswilhelm/personal-website), but Circle kept giving me a "Process exited with code: ExitFailure (-9) (THIS MAY INDICATE OUT OF MEMORY)" error. I decided it was more valuable to get my site up than to get it running on CI, so I added a new site on netlify without Git integration, added [netlify-cli](https://www.npmjs.com/package/netlify-cli) to my project as a development dependency. I am not clear on how authentication works for the CLI, but running `npx netlify init` opened my project in the browser and let me easily give [OAuth](https://oauth.net/) permissions to the CLI, and added a `.netlify` directory to my project. It's got a file in it named `state.json`. This file has a UUID named "siteId" in it. It would be really insecure if that was all it took to deploy to my site, someone would just enumerate all the UUIDs with posts to deploy garbage, but I'm not sure where the CLI stores any more state. I expect it uses my SSH key for authentication, but that's not very clear. If anyone knows the details of it, let me know. For the time being I have deployed successfully to netlify, I updated my DNS settings in cloudfront, and I'm waiting for the [blog.lutra.tech](https://blog.lutra.tech/) CNAME record to propagate.
