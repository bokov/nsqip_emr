#dat01a[1:40000,] %>% #mutate(order00=rank(order00)) %>%
.input %>% mutate(order00=rank(order00)) %>%
{ggplot(.,aes(x=TIME_TO_EVENT,y=order00,group=CASE_DEID,shape=src_evt)) + 
    geom_line(alpha=0.1) + 
    geom_point(data=subset((.),!evt_type %in% c('Admit','Discharge','Surg') &
                             TIME_TO_EVENT < .xlim[2] &
                             TIME_TO_EVENT > .xlim[1])
               ,aes(color=src_evt),size=1,alpha=0.2) + 
    geom_point(data=subset((.),evt_type == 'Discharge' & 
                             TIME_TO_EVENT < .xlim[2] &
                             TIME_TO_EVENT > .xlim[1])
               ,aes(color=src_evt),size=1) + 
    geom_point(data=subset((.),evt_type == 'Surg' &
                             TIME_TO_EVENT < .xlim[2] &
                             TIME_TO_EVENT > .xlim[1])
               ,aes(color=src_evt),size=0.5) + 
    geom_point(data=subset((.),!evt_type %in% c('Admit','Discharge','Surg') &
                             TIME_TO_EVENT<0 &
                             TIME_TO_EVENT > .xlim[1])
               ,color='salmon',size=1,alpha=1) + 
    geom_point(data=subset((.),evt_type == 'Admit' & TIME_TO_EVENT!=0 &
                             TIME_TO_EVENT > .xlim[1] &
                             TIME_TO_EVENT < .xlim[2])
               ,aes(color=src_evt),size=0.5) + 
    scale_shape_manual(values=c(1,2,20,2,20,20,20,20,3,4,20)) +
    scale_color_manual(values=c('red','darkgreen','orange','darkseagreen'
                                ,'orange','orange','orange','green'
                                ,'purple','purple','orange')) +
    scale_x_continuous(limits = .xlim,oob=squish) +
    xlab('Days from NSQIP Admission Date') +
    guides(color=guide_legend('Source|Event')
           ,shape=guide_legend('Source|Event')) +
    theme(axis.text.y=element_blank(),axis.title.y=element_blank()
          ,axis.ticks.y=element_blank()
          ,text=element_text(family="Times New Roman")
          ,legend.position = c(1,1),legend.justification = c('right','top'))
  } %>% print
