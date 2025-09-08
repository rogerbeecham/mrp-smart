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
    "RB plans the RSOS paper",
    
    # Phase 2 tasks
    "RB + SDF data scientists build FINDS MRP model",
    "HASP + SDF data scientist exchange meetings",
    "RB presents at AAG",
    
    # Phase 3 tasks
    "SDF data scientists + Project Manager on-board\t
    data products to FINDS catalogue",
    "RB + HASP data scientists\t
    write EPB Data:Code paper",
    "RB submits methods paper to RSOS",
    
    # Phase 4 tasks
    "RB + SDF data scientists build uncertainty vis\t
    into Economic Wellbeing Explorer",
    "SDF Product Manager + Digital Content/Storytelling Specialist\t
    develop and publicise explanatory data stories"
  ),
  
  start = as.Date(c(
    # Phase 1 (Oct-Dec 2025)
    "2025-10-01", "2025-11-01", "2025-11-15",
    
    # Phase 2 (Jan-Mar 2026) 
    "2026-01-01", "2026-02-01", "2026-03-15",
    
    # Phase 3 (Apr-Jun 2026)
    "2026-04-01", "2026-05-15", "2026-06-15",
    
    # Phase 4 (Jul-Sep 2026)
    "2026-04-15", "2026-08-01"
  )),
  
  end = as.Date(c(
    # Phase 1
    "2026-04-01", "2025-12-15", "2025-12-31", 
    
    # Phase 2
    "2026-03-31", "2026-04-15", "2026-03-21",
    
    # Phase 3
    "2026-06-15", "2026-06-30", "2026-06-30",
    
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
             linetype = "dashed", color = "gray60", alpha = 0.7) +
  
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
  annotate("richtext", x=-.05,y=.98, hjust="left",vjust="top", label.colour = NA, size=4.5, colour="#525252", fill="transparent",
           label="**Phase 1**<br>Code repositories<br> and postratification <br>frames" ) +
  annotate("richtext", x=-.05,y=.75, hjust="left",vjust="top", label.colour = NA, size=4.5, colour="#525252", fill="transparent",
           label="**Phase 2**<br>MRP model <br> development<br> and exchange" ) +
  annotate("richtext", x=-.05,y=.51, hjust="left",vjust="top", label.colour = NA, size=4.5, colour="#525252", fill="transparent",
           label="**Phase 3**<br>MRP roll-out<br> and write-up" ) +
  annotate("richtext", x=-.05,y=.28, hjust="left",vjust="top", label.colour = NA, size=4.5, colour="#525252", fill="transparent",
           label="**Phase 4**<br>Disseminate" ) +
  
  annotate("segment",  x=0.33,y=0.115, xend=0.33, yend=0.01, colour="#525252", linewidth=.2) +
  
  annotate("richtext", x=.2,y=0.05, hjust="middle",vjust="top", label.colour = NA, size=4, colour="#525252", fill="transparent",
           label="2025" ) +
  
  annotate("richtext", x=.6,y=0.05, hjust="middle",vjust="top", label.colour = NA, size=4, colour="#525252", fill="transparent",
           label="2026" ) +
  
  scale_y_continuous(limits=c(-0.05,1.05), expand=c(0, 0)) +
  scale_x_continuous(limits=c(-.1,1.05), expand=c(0, 0)) +
  theme(
    axis.text=element_blank(), 
    axis.line = element_blank(), 
    axis.title.x = element_blank(), axis.title.y = element_blank())

quartz(file = here("figs", "workplan.png"), type = "png", dpi = 300, width = 11, height = 6)
print(p_combined)
dev.off()

