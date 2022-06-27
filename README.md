# paper-cubble

The repo contains:

* `arxiv/`: an initial arxiv submission
* `code/`: codes for reproducibility for both the main article and the appendix. Commented codes are used to generate the data or interactive graphics.
* `data/`: the data used in the article. The scripts to generate these data are also available. See the commented code in reproducible scripts (main & appendix).
* `figures/`: figures used in the article, including diagram illustrations and figures
* `jss/`: the main folder used to create the article and the appendix
* `*.Rmd`: individual rmarkdown files for each section



* The paper requires 

    -  the `strayr` package that can be installed with `remotes::install_github("runapp-aus/strayr")`

* When making changes, use the numbered rmarkdown file in the top level: `1-intro.Rmd`, `2-0-cube.Rmd` etc. To render the pdf, knit on the the master files `jss/cubble-jss.Rmd` and `arxiv/cubble-arxiv.Rmd`, which will render the numbered child rmarkdown files as children.