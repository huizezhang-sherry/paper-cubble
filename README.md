# paper-cubble

* The paper requires 

    -  the `strayr` package that can be installed with `remotes::install_github("runapp-aus/strayr")`

* When making changes, use the numbered rmarkdown file in the top level: `1-intro.Rmd`, `2-0-cube.Rmd` etc. To render the pdf, knit on the the master files `jss/cubble-jss.Rmd` and `arxiv/cubble-arxiv.Rmd`, which will render the numbered child rmarkdown files as children.