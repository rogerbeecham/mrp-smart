library(tidyverse)
library(lubridate)
library(here)
library(ggtext)
library(patchwork)
library(cowplot)
source(here("code", "theme_clean.R"))

library(systemfonts)
library(ragg)
system_fonts() |> filter(grepl("Jost", family, ignore.case = TRUE))
#options(device = ragg::agg_png)


# Project data based on your work plan
project_data <- data.frame(
  task = c(
    # Phase 1 tasks
    "RB develops MRP code repositories",
    "RB constructs postratification frames +\t
    area-level context variables for FINDS model",
    "RB works on the RSOS methods paper",
    
    # Phase 2 tasks
    "RB + SDF data scientists build FINDS MRP model",
    "HASP + SDF data scientist exchange meetings",
    "RB presents at AAG 2026",
    
    # Phase 3 tasks
    "SDF data scientists + Project Manager on-board\t
    data products to FINDS catalogue",
    "RB + HASP data scientists develop\t
    and submit EPB Data:Code paper",
    "RB submits methods paper to RSOS",
    
    # Phase 4 tasks
    "RB + SDF data scientists build uncertainty vis\t
    into Economic Wellbeing Explorer",
    "SDF Product Manager + Digital Content/Storytelling Specialist\t
    develop and publicise explanatory data stories"
  ),
  
  start = as.Date(c(
    # Phase 1 (Oct-Dec 2025)
    "2025-10-01", "2025-11-01", "2025-11-01",
    
    # Phase 2 (Jan-Mar 2026) 
    "2025-12-15", "2026-01-15", "2026-03-15",
    
    # Phase 3 (Apr-Jun 2026)
    "2026-04-01", "2026-05-15", "2026-05-15",
    
    # Phase 4 (Jul-Sep 2026)
    "2026-04-15", "2026-08-01"
  )),
  
  end = as.Date(c(
    # Phase 1
    "2026-04-01", "2025-12-15", "2026-06-15", 
    
    # Phase 2
    "2026-03-31", "2026-04-15", "2026-03-21",
    
    # Phase 3
    "2026-06-15", "2026-08-01", "2026-06-15",
    
    # Phase 4
    "2026-09-15", "2026-09-30"
  )),
  
  phase = c(
    # Phase labels
    rep("p1", 3),
    rep("p2", 3), 
    rep("p3", 3),
    rep("p4", 2)
  )) |> 
  mutate(task_num=row_number())

# Create the Gantt chart
p <- ggplot(project_data, aes(x = as_date(start), xend = as_date(end), 
                                        y = reorder(task, -task_num), 
                                        yend = reorder(task, -task_num), colour=phase, fill=phase)) +
  geom_segment(size = 5) +
  geom_text(
    data=. %>% filter(start>"2026-03-01"), 
    aes(x=as_date(start)-days(8), label=task), 
    #letter_spacing = unit(-0.5, "pt"),
    hjust="right", colour="#525252", family="Jost", fontface="italic", size=3.5) +
  
  geom_text(
    data=. %>% filter(start<"2026-03-01"), 
    aes(x=as_date(end)+days(8), label=task), 
    #letter_spacing = unit(-0.5, "pt"),
    hjust="left", colour="#525252", family="Jost", fontface="italic", size=3.5) +
  # geom_richtext(
  #   data=. %>% filter(start>"2026-03-01"), 
  #   aes(x=as_date(start)-days(8), label=task), 
  #   #letter_spacing = unit(-0.5, "pt"),
  #   hjust="right", label.colour = NA, colour="#525252", fill="transparent", family="Jost") +
  geom_point(aes(x = start), size = 3.5, shape = 21, stroke = 1) +
  geom_point(aes(x = end), size = 3.5, shape = 21, stroke = 1) +
  
  scale_colour_manual(values=c("#a6cee3", "#fdbf6f", "#b2df8a", "#fb9a99")) +
  scale_fill_manual(values=c("#a6cee3", "#fdbf6f", "#b2df8a", "#fb9a99")) +
  
  # Add phase separators
  geom_hline(yintercept = c(2.5, 5.5, 8.5), 
             linetype = "dashed", colour = "#525252", alpha = 0.4, linewidth=.5) +
  
  # Add quarter separators
  geom_vline(xintercept = c(as_date("2025-12-15"), as_date("2026-03-15"), as_date("2026-06-15")), 
             linewidth=.2, colour = "#525252", alpha = 0.4) +
  
  # Formatting
  scale_x_date(date_breaks = "1 month", 
               # date_labels = "%b\n %Y",
               date_labels = "%b",
               limits = c(as.Date("2025-09-01"), as.Date("2026-10-01"))) +
  
  labs(x="", y="") +
  guides(colour="none", fill="none") +
  theme(
    axis.text.y=element_blank(), axis.line.y=element_blank())

p_combined <- ggplot() +
  annotation_custom(as_grob(p), xmin = 0, ymin = 0, xmax = 1, ymax = 1) +
  
  annotate("richtext", x=-.06,y=1.11, hjust="left",vjust="top", label.colour = NA, size=6.5, colour="#525252", fill="transparent",
           label="**Project plan**" ) +
  
  annotate("richtext", x=-.05,y=.98, hjust="left",vjust="top", label.colour = NA, size=4.5, colour="#525252", fill="transparent",
           label="**Phase 1**<br>Code repositories<br> and postratification <br>frames" ) +
  annotate("richtext", x=-.05,y=.75, hjust="left",vjust="top", label.colour = NA, size=4.5, colour="#525252", fill="transparent",
           label="**Phase 2**<br>MRP model <br> development<br> and exchange" ) +
  annotate("richtext", x=-.05,y=.51, hjust="left",vjust="top", label.colour = NA, size=4.5, colour="#525252", fill="transparent",
           label="**Phase 3**<br>MRP roll-out<br> and write-up" ) +
  annotate("richtext", x=-.05,y=.28, hjust="left",vjust="top", label.colour = NA, size=4.5, colour="#525252", fill="transparent",
           label="**Phase 4**<br>Disseminate" ) +
  
  annotate("segment",  x=0.317,y=0.115, xend=0.317, yend=0.01, colour="#525252", linewidth=.2) +
  
  annotate("richtext", x=.2,y=0.05, hjust="middle",vjust="top", label.colour = NA, size=4, colour="#525252", fill="transparent",
           label="2025" ) +
  
  annotate("richtext", x=.6,y=0.05, hjust="middle",vjust="top", label.colour = NA, size=4, colour="#525252", fill="transparent",
           label="2026" ) +
  
  annotation_custom(as_grob(p_person), xmin = -.05, ymin = -.15, xmax = .35, ymax = -.5) +
  
  annotate("richtext", x=-.065,y=-.04, hjust="left",vjust="top", label.colour = NA, size=6.5, colour="#525252", fill="transparent",
           label="**Personnel allocation**" ) +
  
  annotate("richtext", x=.048,y=-.12, hjust="middle",vjust="top", label.colour = NA, size=4, colour="#525252", fill="transparent",
           label="HASP" ) +
  
  annotate("richtext", x=.2,y=-.12, hjust="middle",vjust="top", label.colour = NA, size=4, colour="#525252", fill="transparent",
           label="SDF" ) +
  
  annotate("richtext", x=.37,y=-.38, hjust="left",vjust="top", label.colour = NA, size=3.3, colour="#525252", fill="transparent",
           label="*3xDS column*<br>HASP Data Scientists are<br>not directly costed." ) +
  
  annotate("richtext", x=.37,y=-.18, hjust="left",vjust="top", label.colour = NA, size=3.3, colour="#525252", fill="transparent",
           label="Circles are sized proportionally<br>to time-adjusted workload. <br> So RB is working 22% over<br>12 months, SDF data scientists<br> 40% in 9 months." ) +
  
  
 # annotate("curve", x=.34, xend=.09, y=-.43, yend=-.43, linewidth=.2, colour="#969696", arrow = arrow(length = unit(0.006, "npc")), curvature = -0.3) + 
  
  annotate("segment",  x=0.0895,y=-0.14, xend=0.0895, yend=-0.22, colour="#969696", linewidth=.15) +
  
  scale_y_continuous(limits=c(-0.5,1.11), expand=c(0, 0)) +
  scale_x_continuous(limits=c(-.1,1.05), expand=c(0, 0)) +
  theme(
    axis.text=element_blank(), 
    axis.line = element_blank(), 
    axis.title.x = element_blank(), axis.title.y = element_blank())

quartz(file = here("figs", "workplan.png"), type = "png", dpi = 300, width = 11, height = 8.5)
print(p_combined)
dev.off()


person_data <- data.frame(
  phase = rep(c("P1","P2", "P3", "P4"), times=7, each=1),
  person = rep(c("RB", 
                   "3xDS", 
                   "DS", 
                   "SDS", 
                   "CDS", 
                   "PM", 
                   "DC&S"),
                   times=1, each=4),
  time=c(
    22,22,22,22,
    0, 10, 10, 10,
    0, 40, 40, 40,
    0, 40, 40, 40,
    0, 5, 5, 5,
    0, 0, 3, 10,
    0, 0, 0, 13
    )
  ) |> 
  mutate(person=factor(person, levels=c("RB", 
                       "3xDS", 
                       "DS", 
                       "SDS", 
                       "CDS", 
                       "PM", 
                       "DC&S")),
         phase=factor(phase, levels=c("P1","P2", "P3", "P4")))

p_person <- person_data |> 
  mutate(phase=fct_rev(phase)) |> 
  ggplot(aes(y=phase,x=person)) +
  geom_tile(colour="#969696", fill="transparent", linewidth=.1) +
  #geom_tile(data = . %>% filter(person!="3xDS"), colour="#f0f0f0", fill="transparent", linewidth=.5) +
  #geom_tile(data = . %>% filter(person=="3xDS"), colour="#f0f0f0", fill="#f0f0f0", linewidth=.5) +
  geom_point(data = . %>% filter(time>0, person!="3xDS"), aes(size=time, fill=phase, colour=phase), alpha=1, pch=21) +
  geom_point(data = . %>% filter(time>0, person=="3xDS"), 
             aes(size=time, colour=phase, fill=phase), alpha=.5, pch=21) +
  scale_colour_manual(values=c("#fb9a99", "#b2df8a", "#fdbf6f", "#a6cee3")) +
  scale_fill_manual(values=c("#fb9a99", "#b2df8a", "#fdbf6f", "#a6cee3")) +
  scale_x_discrete(position = "top") +
  guides(fill="none", colour="none", size="none") +
  theme(
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_text(face="bold")
  )
  
  
  time = c()
  )
                 
                 
                 
             rep("3xDS\nHASP", times=4, each=1), 
             rep("DS\nSDF", times=4, each=1), 
             rep("SDS\nSDF", times=4, each=1),
             rep("CDS\nSDF",times=4, each=1), 
             rep("PM\nSDF",times=4, each=1), 
             rep("DC&S\nSDF",times=4, each=1))
  ) 


