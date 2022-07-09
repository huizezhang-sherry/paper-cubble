# paper-cubble

The repo contains:

* `arxiv/`: an initial arxiv submission
* `code/`: codes for reproducibility for both the main article and the appendix. Commented codes are used to generate the data or interactive graphics.
* `data/`: the data used in the article. The scripts to generate these data are also available. See the commented code in reproducible scripts (main & appendix).
* `figures/`: figures used in the article, including diagram illustrations and figures
* `jss/`: the main folder used to create the article and the appendix
* `*.Rmd`: individual rmarkdown files for each section

The current working rule used to markup: 
  * use \pkg{} when refers to the package 
  * use \code{} when refers to the class or object i.e., the cubble object, the long/ nested cubble)
  * whenever possible, avoid using a bare cubble
  * ggplot is not marked up currently  