# Chris Dance

A minimal Markdown blog for GitHub Pages. No theme or JavaScript.

## Write an article

Add a Markdown file to `_posts/`, named `YYYY-MM-DD-your-title.md`:

```markdown
---
title: Your article title
description: A short description of the article.
---

Write your article here in Markdown.

## A heading

More text, **bold**, *italics*, and [links](https://example.com).
```

Articles appear automatically on the homepage, newest first. Future-dated posts
are omitted until the site is rebuilt on or after their date. The filename sets
the article URL: `/articles/your-title/`.

Edit `about.md` for the About page, `_config.yml` for the site name and description,
and `style.css` for the styling. Replace or delete the sample Lorem ipsum post when
you are ready.

## GitHub Pages

In the repository's **Settings → Pages**, use **Deploy from a branch** and select
the branch containing this site and the **/ (root)** folder. GitHub Pages builds
the Markdown with Jekyll when changes are pushed.

## Preview locally

With Ruby and Bundler installed:

```sh
bundle install
bundle exec jekyll serve
```

Open `http://localhost:4000`. Generated files go into `_site/` and are ignored by Git.
