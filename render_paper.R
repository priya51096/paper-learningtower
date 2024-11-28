# To render the paper requires using command line
pandoc::pandoc_activate(version = '3.1.6')
rmarkdown::render('learningtower.Rmd', output_format = 'all')
