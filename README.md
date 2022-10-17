# Staticly

An opinionated Static-Site Generator associating Clojure, Hiccup, Markdown and Tailwind with the following stated goals:

* Use Clojure's **Hiccup format** and pure clojure function for composition and templating
* Use Markdown and provides utility to **process markdown effectively** (a la Markdoc)
* **Good developer experience**: Reload the browser's tab(s) displaying the rendered HTML file(s) whenever a change is made to either clojure code or markdown files (so no dev Server logic in order to keep the code simple)
* **Tailwind CSS friendly**, as it's a very efficient way of doing styling.
* **Target static page hosting** with a CI/CD mechanism like [Cloudflare Pages](https://pages.cloudflare.com) that will build and host your HTML files (example in section [Cloudflare Hosting](#cloudflare-hosting))

# Usage

## Rendering with everything in Clojure code

You write a `render` function with no argument that just output a Hiccup data structure, you're free to use any data structure and way of structuring (or not) the content. Then you add the `def-render-builder` macro invocation at the end of the namespace that will:

* From the hiccup returned by the `render` function, export the HTML in the `resources/public` directory with the namespace last name as the filename with the `.html` suffix,
* Reload the browser's tab(s) that have the namespace first name in the url,
* E.g. the `mywebsite.index` namespace will export the rendered HTML in the `resources/public/index.html` file and reload the tab with `mywebsite` in its URL,
* Add a `build!` function in that namespace that will do the export and reload steps describe above.


```clojure

(require 'defsquare.staticly)

(defn head []
 ...)

(defn body []
 ...)

(defn footer []
 ...)

(defn render []
[:html {:lang "en"}
 (head)
 (body)
 (footer)])
 
 (staticly/def-render-builder)

```


## Page rendering with Markdown files

The idea is to have a markdown file (with a front-matter in Yaml or EDN) associated with the template.
1-arg `page-template` function with map as the first argument:



Staticly provices a macro `def-blog-builder` you should put at the end of your namespace that will:
- define a `build!` function to invoke whenever
*

## Blog rendering with Markdown files

two "template" functions to implement :

* `post-template`
* `home-template`

Staticly provides a macro `def-page-builder` that:
*

# hosting

## Cloudflare hosting

You should have a dedicated shell script that will be invoked by the Cloudflare CI associated with a `build` namespace that will invoke all the included namespace.
E.g.:

```clojure
(defn -main [& args]
  (index/build!)
  (blog/build!)
  (pages/build!)
  (shutdown-agents))
```
 
