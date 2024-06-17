(ns defsquare.markdown
  (:require
   [camel-snake-kebab.core :as csk]
   [clj-yaml.core :as yaml]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hickory.core :as hickory]
   [instaparse.core :as insta]
   [markdown.common :as common ]
   [markdown.core :as md]
   [markdown.links :as links ]
   [markdown.lists :as lists ]
   [markdown.tables :as tables ]
   [markdown.transformers :as transformers]
   [nextjournal.markdown :as next.md]
   [nextjournal.markdown.transform :as next.md.transform]
   [defsquare.files :as file-utils])
  (:refer-clojure :exclude [read])
  (:import
   [java.text Normalizer]))


(defn- add-hiccup [{:keys [html] :as markdown}]
  (assoc markdown :hiccup (doall (map hickory/as-hiccup (hickory/parse-fragment html)))))

(defn ->hiccup [hiccup-renderers data]
  (next.md.transform/->hiccup
   (merge next.md.transform/default-hiccup-renderers hiccup-renderers)
   data))

(def parse next.md/parse)
(def into-markup next.md.transform/into-markup)

(defn normalize [s]
  (when s
    (-> s
        (Normalizer/normalize java.text.Normalizer$Form/NFD)
        (.replaceAll "[^\\p{ASCII}]|['`\";:?//.,+=&*~]" "")
        (csk/->kebab-case-string))))

(defn heading-text [text]
  (-> (str/replace text #"^([ ]+)?[#]+" "")
      (str/replace #"[#]+$" "")
      str/trim))

(defn heading-level [text]
  (let [num-hashes (count (filter #(not= \space %) (take-while #(or (= \# %) (= \space %)) (seq text))))]
    (when (pos? num-hashes) num-hashes)))

(defn make-heading [text heading-anchors]
  (when-let [heading (heading-level text)]
    ;(println text heading)
    (let [text (heading-text text)]
      (str "<h" heading " id=\"" (normalize text) "\">" text "</h" heading ">"))))

(defn heading? [text type]
  (when-not (every? #{\space} (take 4 text))
    (let [trimmed (some-> text str/trim)]
      (and (not-empty trimmed) (every? #{type} trimmed)))))

(defn h1? [text]
  (heading? text \=))

(defn h2? [text]
  (heading? text \-))

(defn heading-with-id [text {:keys [buf next-line code codeblock heading-anchors] :as state}]
  (cond
    (or codeblock code)
    [text state]

    (h1? (or buf next-line))
    [(str "<h1 id=\"" (normalize text) "\">" text "</h1>") (assoc state :heading true)]

    (h2? (or buf next-line))
    [(str "<h2 id=\"" (normalize text) "\">" text "</h2>") (assoc state :heading true)]

    :else
    (if-let [heading (make-heading text heading-anchors)]
      [heading (assoc state :inline-heading true)]
      [text state])))


(defn process
  "return a map with three keys `:metadata`, `:raw` and `:hiccup` with respectively the meta found in the md file front-matter, the markdown content and the markdown content transformed to hiccup"
  [^java.lang.String s]
  ;;IMPORTANT the transformers needs to stay in that order
  (when s
    (-> (md/md-to-html-string-with-meta s :replacement-transformers [transformers/set-line-state
                                                                     transformers/empty-line
                                                                     common/inhibit
                                                                     common/escape-inhibit-separator
                                                                     transformers/code
                                                                     transformers/codeblock
                                                                     common/escaped-chars
                                                                     common/inline-code
                                                                     transformers/autoemail-transformer
                                                                     transformers/autourl-transformer
                                                                     links/image
                                                                     links/image-reference-link
                                                                     links/link
                                                                     links/implicit-reference-link
                                                                     links/reference-link
                                                                     links/footnote-link
                                                                     transformers/hr
                                                                     transformers/blockquote-1
                                                                     lists/li
                                                                     heading-with-id;this is a custom transformers to include anchor with id from the title (uuid in original code, should make a PR...)
                                                                     transformers/blockquote-2
                                                                     common/italics
                                                                     common/bold-italic
                                                                     common/em
                                                                     common/strong
                                                                     common/bold
                                                                     common/strikethrough
                                                                     transformers/superscript
                                                                     tables/table
                                                                     transformers/paragraph
                                                                     transformers/br
                                                                     common/thaw-strings
                                                                     common/dashes
                                                                     transformers/clear-line-state])
        (add-hiccup)
        (assoc :raw s))))

(defn process-file [f]
  (assoc (process (slurp (io/as-file f)))
         :file f
         :path (file-utils/parse-path f)))

(defn process-files [dir includes excludes]
  (let [files (file-utils/list-files dir includes excludes)]
     (map process-file files)))

(defn process-dir [dir]
  (let [files (file-utils/list-files dir file-utils/markdown?)]
    (map process-file files)))

(defn read [dirs])

(defn parse-yaml-metadata-headers
  [lines-seq]
  (->> lines-seq
       ;; leave off opening ---
       (drop 1)
       ;; take lines until we see the closing ---
       (take-while (comp not (partial re-matches #"---\s*")))
       ;; join together and parse
       (str/join "\n")
       yaml/parse-string))

(defn drop-yaml-metadata-headers
  [lines-seq]
  (->> lines-seq
       ;; leave off opening ---
       (drop 1)
       ;; drop lines until we see the closing ---
       (drop-while (comp not (partial re-matches #"---\s*")))
       (drop 1)))

(defn parse-edn-metadata-headers
  [lines-seq]
  (->> lines-seq
       ;; take sequences until you hit an empty line
       (take-while (comp not (partial re-matches #"\s*")))
       ;; join together and parse
       (str/join "\n")
       edn/read-string))

(defn parse-metadata-headers
  "Given a sequence of lines from a markdown document, attempt to parse a
  metadata header if it exists. Accepts wiki, yaml, and edn formats."
  [lines-seq]
  {:pre [(sequential? lines-seq)
         (every? string? lines-seq)]}
  (cond
    ;; Treat as yaml
    (re-matches #"--- *" (first lines-seq))
    (parse-yaml-metadata-headers lines-seq)
    ;; Treat as edn
    (re-matches #"\{.*" (first lines-seq))
    (parse-edn-metadata-headers lines-seq)))

(defn- s2sr [s]
  (-> s (java.io.StringReader.) (java.io.BufferedReader.)))

(defn parse-metadata-str [s]
  (let [br (s2sr s)]
    (parse-metadata-headers (line-seq br))))

(defn drop-metadata-str [s]
  (let [br (s2sr s)]
    (->> s
        s2sr
        line-seq
        drop-yaml-metadata-headers
        (interpose "\n")
        (apply str))))

(parse-metadata-str "---
key1: value1
key2: value2
---

# heading 1 ")

(drop-metadata-str "---
key1: value1
key2: value2
---

# heading 1 ")

(defn parse-metadata [filename]
  (let [file (io/as-file filename)]
    (with-open [rdr (io/reader file)]
      (assoc {}
             :metadata (parse-metadata-headers (line-seq rdr))
             :file file
             :path (file-utils/parse-path file)))))

(defn list-markdowns-with-meta
  "return a map of path string of every md files in a dir to a map with the front mattter found in each files and :file and :path"
  ([dir]
   (map parse-metadata (file-utils/list-files dir file-utils/markdown?)))
  ([dir re-filename-to-match]
   (let [files (file-utils/list-files dir (partial file-utils/re-match-filename? re-filename-to-match))]
     (map parse-metadata files)))
  ([dir includes excludes]
   (let [files (file-utils/list-files dir includes excludes)]
     (map parse-metadata files))))


;(all-markdowns-with-meta "blog")
;(md/parse-metadata "blog/post1.md")
;(file-utils/list-files "blog" file-utils/markdown?)
;(file-utils/list-files "blog")
;(def md-test-file (io/resource "test.md"))
;(def md-test-string (slurp md-test-file))

(def md-to-tree (insta/parser
   "<root> = (heading |
              unordered-list |
              ordered-list |
              pre-code |
              inline-code |
              anchor |
              image |
              paragraph
             )+
    heading = #'[#]+' <space> #'[a-zA-Z0-9 ]+' <blankline>?
    paragraph = (inline-code |
                 anchor |
                 strong |
                 emphasis |
                 paragraph-text
                )+ <#'\n\n'>
    <paragraph-text> = #'[^`#*\n{2}]+'
    strong = <'**'> strong-text <'**'>
    <strong-text> = #'[^\\*\\*]+'
    emphasis =  <'*'> emphasis-text <'*'>
    <emphasis-text> = #'[^\\*]+'
    unordered-list = unordered-item+ <blankline>
    unordered-item = <'- '> #'[a-zA-Z ]+' <newline>?
    ordered-list = ordered-item+ <blankline>
    ordered-item = <ol-item-token> #'[a-zA-Z0-9 ]+' <newline>?
    ol-item-token = #'[0-9]+\\. '
    inline-code = <'`'> #'[^`]+' <'`'>
    pre-code = <'~~~'> lang? <newline>
               codetext
               <'\n~~~'> <blankline>
    lang = <' '> #'[a-zA-Z]+'
    codetext = #'[^\\n~~~]+'
    anchor = auto-anchor | braced-anchor
    <auto-anchor> = <'<'> url <'>'>
    <braced-anchor> = <'['> text <']'> <'('> url <')'>
    <text> = #'[^]]+'
    <url> = #'[^>)]+'
    image = <'!'>
            <'['> alt <']'>
            <'('> path title? <')'>
    <alt> = #'[^]]+'
    <path> = #'[^) ]+'
    <title> = <spaces> #'[^)]+'
    spaces = space+
    space = ' '
    blankline = #'\n\n'
    newline = #'\n'
    "))

(def parse-md (insta/parser
    "<Blocks> = (Header | Paragraph | List | Ordered | Code | Rule | <Blankline>)+
    Header = Blankline* Headerline Blankline+
    <Headerline> = h1 | h2 | h3
    h1 = '#' (Whitespace Word)+ EOL
    h2 = '##'
    h3 = '###'
    List = Listline+ Blankline+
    Listline = Listmarker Whitespace+ Word (Whitespace Word)* EOL
    <Listmarker> = <'+' | '*' | '-'>
    Ordered = Orderedline+ Blankline+
    Orderedline = Orderedmarker Whitespace* Word (Whitespace Word)* EOL
    <Orderedmarker> = <#'[0-9]+\\.'>
    Code = Codeline+ Blankline+
    Codeline = <Space Space Space Space> (Whitespace | Word)* EOL
    Rule = Ruleline Blankline+
    <Ruleline> = <'+'+ | '*'+ | '-'+>
    Paragraph = Line+ Blankline+
    <Blankline> = Whitespace* EOL
    <Line> = Linepre Word (Whitespace Word)* Linepost EOL
    <Linepre> = (Space (Space (Space)? )? )?
    <Linepost> = Space?
    <Whitespace> = #'\\s+'
    <Space>      = ' '  | '\t'
    <EOL>        = #'\r?\n'
    <Word> = #'\\S+'"))

#_(do (def parse-md-blocks (insta/parser
   "<Blocks>      = (Section | <Blankline> | Block)+
    Section       = H1 Blankline+ (Subsection | Block)*
    Subsection    = H2 Blankline+ (Subsubsection | Block)*
    Subsubsection = H3 Blankline+ Block*
    H1            = Whitespace* '#'   (Whitespace Word)+ <EOL> (Blankline | Whitespace)*
    H2            = Whitespace* '##'  (Whitespace Word)+ <EOL> (Blankline | Whitespace)*
    H3            = Whitespace* '###' (Whitespace Word)+ <EOL> (Blankline | Whitespace)*
    Block         = Line+ (Blankline | Whitespace)*
    <Blankline>   = Whitespace* EOL+
    <Line>        = Whitespace* !('#'|'##'|'###') (Whitespace | Word)+ EOL
    <Whitespace>  = #'[^\\S\n]+'
    <EOL>         = #'\r?\n'
    <Word>        = #'^\\S+'"))
    (parse-md-blocks  (slurp "resources/simple.md")))

(do (def parse-md-blocks (insta/parser
   "<Blocks>      = (section | <Blankline> | block)+
    section       = H1 Blankline+ (subsection | subsubsection | block)*
    subsection    = H2 Blankline+ (subsubsection | block)*
    subsubsection = H3 Blankline+ block*
    H1            = Whitespace* '#'   (Whitespace Word)+ <EOL> (Blankline | Whitespace)*
    H2            = Whitespace* '##'  (Whitespace Word)+ <EOL> (Blankline | Whitespace)*
    H3            = Whitespace* '###' (Whitespace Word)+ <EOL> (Blankline | Whitespace)*
    block         = Line+ (Blankline | Whitespace)*
    <Blankline>   = Whitespace* EOL+
    <Line>        = Whitespace* !('#'|'##'|'###') (Whitespace | Word)+ EOL
    <Whitespace>  = #'[^\\S\n]+'
    <EOL>         = #'\r?\n'
    <Word>        = #'^\\S+'"))

    (def parsed (parse-md-blocks "
# Heading 1

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla elementum, massa id rhoncus ultrices, velit nulla facilisis lectus, ac lobortis purus tellus quis velit. Nam a nunc a neque iaculis vulputate eu ut ligula. Morbi consequat in ipsum eu auctor. Praesent posuere tortor cursus egestas viverra. Etiam in scelerisque est. Aenean ornare ullamcorper est, in aliquet sapien lobortis ut. Nunc imperdiet lacus et venenatis congue. Suspendisse ac sollicitudin tortor. Pellentesque bibendum tristique velit a laoreet. Pellentesque luctus nisi luctus, faucibus dolor nec, porttitor nisl. Suspendisse hendrerit elementum lacinia. Nullam egestas sapien a erat hendrerit, ut cursus magna varius.

## Heading 1a

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla elementum, massa id rhoncus ultrices, velit nulla facilisis lectus, ac lobortis purus tellus quis velit. Nam a nunc a neque iaculis vulputate eu ut ligula. Morbi consequat in ipsum eu auctor. Praesent posuere tortor cursus egestas viverra. Etiam in scelerisque est. Aenean ornare ullamcorper est, in aliquet sapien lobortis ut. Nunc imperdiet lacus et venenatis congue. Suspendisse ac sollicitudin tortor. Pellentesque bibendum tristique velit a laoreet. Pellentesque luctus nisi luctus, faucibus dolor nec, porttitor nisl. Suspendisse hendrerit elementum lacinia. Nullam egestas sapien a erat hendrerit, ut cursus magna varius.

### Heading 1a'

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla elementum, massa id rhoncus ultrices, velit nulla facilisis lectus, ac lobortis purus tellus quis velit. Nam a nunc a neque iaculis vulputate eu ut ligula. Morbi consequat in ipsum eu auctor. Praesent posuere tortor cursus egestas viverra. Etiam in scelerisque est. Aenean ornare ullamcorper est, in aliquet sapien lobortis ut. Nunc imperdiet lacus et venenatis congue. Suspendisse ac sollicitudin tortor. Pellentesque bibendum tristique velit a laoreet. Pellentesque luctus nisi luctus, faucibus dolor nec, porttitor nisl. Suspendisse hendrerit elementum lacinia. Nullam egestas sapien a erat hendrerit, ut cursus magna varius.

### Heading 1a''

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla elementum, massa id rhoncus ultrices, velit nulla facilisis lectus, ac lobortis purus tellus quis velit. Nam a nunc a neque iaculis vulputate eu ut ligula. Morbi consequat in ipsum eu auctor. Praesent posuere tortor cursus egestas viverra. Etiam in scelerisque est. Aenean ornare ullamcorper est, in aliquet sapien lobortis ut. Nunc imperdiet lacus et venenatis congue. Suspendisse ac sollicitudin tortor. Pellentesque bibendum tristique velit a laoreet. Pellentesque luctus nisi luctus, faucibus dolor nec, porttitor nisl. Suspendisse hendrerit elementum lacinia. Nullam egestas sapien a erat hendrerit, ut cursus magna varius.

# Heading 2

### Heading 3

")))

(parse-md-blocks "# heading 1

### Heading 3a'

To produce a code block in Markdown, simply indent every line of the
block by at least 4 spaces or 1 tab.

This is a normal paragraph:

    This is a code block.

Here is an example of AppleScript:

    tell application \"Foo\"
        beep
    end tell

A code block continues until it reaches a line that is not indented
(or the end of the article).
Within a code block, ampersands (`&`) and angle brackets (`<` and `>`)
are automatically converted into HTML entities. This makes it very
easy to include example HTML source code using Markdown -- just paste
it and indent it, and Markdown will handle the hassle of encoding the
ampersands and angle brackets. For example, this:

")

(defn- extract-text [sub&Blocks]
  (loop [[tag & remaining] (first sub&Blocks)
         sub&Blocks (rest sub&Blocks)
         acc ""]
    (if (empty sub&Blocks)
      (str acc (apply str (flatten remaining)))
      (recur (first sub&Blocks) (rest sub&Blocks) (str acc (apply str (flatten remaining)))))))




#_(do (def simple-parser (insta/parser "<Blocks>     =  Blankline+
    Blankline  = Whitespace* EOL
    Whitespace = #'\\s+'
    EOL        = #'\r?\n'"))
    (simple-parser "\n\n\n"))


#_(-> (slurp "resources/simple.md")
    parse-md-blocks
    ;sections->tree
    )
#_(do
  (defn sections->tree [sections]
    (into {} (map (fn [[_ [h-level & heading] & rest]]
                    ;(println h-level heading rest)
                    [(apply str (flatten heading)) (extract-text rest)]
                    #_(if (= :H3 h-level)
                        [(apply str (flatten heading)) (extract-text rest)]
                        [heading
                                        ;(sections->tree sub&Blocks)
                                        ;TODO extract text until subsection and invoke recursively on the subsection

                         ])) sections)))

  (sections->tree parsed))
