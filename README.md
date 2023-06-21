# paper-cubble

The repo contains:

* `arxiv/`: an initial arxiv submission
* `code/`: codes for reproducibility for both the main article and the appendix. Commented codes are used to generate the data or interactive graphics.
* `data/`: the data used in the article. The scripts to generate these data are also available. See the commented code in reproducible scripts (main & appendix).
* `figures/`: figures used in the article, including diagram illustrations and figures
* `review/`: comments from the reveiwers and responses

The article is written in `main.Rmd` with reference in `references.bib`, rendered into `main.tex` and `main.pdf` with style files: `jss.bst`, `jss.cls`, `jsslogo.jpg`, and `orcidlink.sty`. 

The current working rule used to markup: 
  * cubble is either marked up using \pkg{} when refers to the package or using \code{} when refers to the class or object.
  * ggplot is not marked up currently  