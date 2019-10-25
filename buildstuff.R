library(rmarkdown);

R_RENDERFILE <- Sys.getenv('R_RENDERFILE');

render(R_RENDERFILE
       ,output_format=html_document(keep_md=TRUE
                                    ,pandoc_args=c('--filter'
                                                   ,'pandoc-crossref')));

# render(R_RENDERFILE
#        ,output_format = word_document(reference_docx = '$styletemplate'
#                                       ,keep_md=TRUE
#                                       ,pandoc_args=c('--filter'
#                                                      ,'pandoc-crossref')));


c()