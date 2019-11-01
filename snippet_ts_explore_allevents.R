#' This is a code snippet specifically for plotting the timelines figures in
#' the main scriport-- too specific to create a function, too lengthy and 
#' obscure to put inline into the main scriport, and yet used several times with
#' only minor changes. So it lives in this separate file.
#' 
#' **usage:** assign a value to `.input` and then do
#' `source('snippet_ts_explore_allevents.R',local=T)`
#' 
#+ main, eval=FALSE, echo=TRUE
.input %>% 
{ggplot(.,aes(x=TIME_TO_EVENT,y=order.active,group=CASE_DEID,shape=src_evt
              ,color=src_evt,size=src_evt)) + 
    geom_line(alpha=0.1,color='black',size=0.5) + 
    geom_point(data=subset((.),src_evt %in% v(c_misc)&
                             TIME_TO_EVENT < .xlim[2] &
                             TIME_TO_EVENT > .xlim[1])
               ,alpha=0.5) + 
    geom_point(data=subset((.),src_evt %in% v(c_prepst) &
                             TIME_TO_EVENT < .xlim[2] &
                             TIME_TO_EVENT > .xlim[1])
               ) + 
    geom_point(data=subset((.),src_evt %in% v(c_dsc) & 
                             TIME_TO_EVENT < .xlim[2] &
                             TIME_TO_EVENT > .xlim[1])
               ) + 
    geom_point(data=subset((.),src_evt %in% v(c_srg) &
                             TIME_TO_EVENT < .xlim[2] &
                             TIME_TO_EVENT > .xlim[1])
               ) + 
    geom_point(data=subset((.),evt_type == 'Admit' & TIME_TO_EVENT!=0 &
                             TIME_TO_EVENT > .xlim[1] &
                             TIME_TO_EVENT < .xlim[2])
               ) + 
    scale_shape_manual(limits=dct0$src_evt,values = dct0$shape) +
    scale_color_manual(limits=dct0$src_evt,values = dct0$color) +
    scale_size_manual(limits=dct0$src_evt,values= dct0$size) +
    scale_x_continuous(limits = .xlim,oob=squish) +
    xlab('Days from NSQIP Admission Date') +
    guides(color=guide_legend('Source|Event',ncol=2)
           ,shape=guide_legend('Source|Event',ncol=2)
           ,size=guide_legend('Source|Event',ncol=2)
           ) +
    theme(axis.text.y=element_blank(),axis.title.y=element_blank()
          ,axis.ticks.y=element_blank()
          ,text=element_text(family="Times New Roman")
          ,legend.position = 'bottom')
  } %>% print
