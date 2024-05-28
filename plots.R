#Create plots of potential energy surface, binding energy on y axis versus atomic coordinates on x axis
library(ggplot2)
library(dplyr)
library(ggrepel)

#Create a data frame with the potential energy surface, which corresponds to the sum of three gaussians
pes <- data.frame(x = seq(-5, 5, length.out = 1001))
pes$y <- with(pes, 1.0 * exp(-(x-0)^2) + 0.5 * exp(-(x - 2)^2) + 0.4 * exp(-(x + 2)^2))
pes$y2 <- with(pes, 0.8 * exp(-(x-0)^2) + 1.0 * exp(-(x - 2)^2) + 1.6 * exp(-(x + 2)^2))
pes$y3 <- with(pes, 0.6 * exp(-(x-0)^2) + 0.2 * exp(-(x - 2)^2) + 0.2 * exp(-(x + 2)^2) + 0.7 * exp(-(x - 3)^2) + 0.65 * exp(-(x + 3)^2))

#Create a data frame with the binding energy, which is the negative of the potential energy surface
binding_energy_ <- data.frame(x = pes$x, y = -pes$y, y2=-pes$y2, y3=-pes$y3)

#Create a plot of the potential energy surface
pes_plot <- ggplot(binding_energy, aes(x = x, y = y)) +
  geom_line() +
  labs(x = "Generalized Coordinates of the System",
       y = "Binding Energy Estimate (Score)") +
  #remove x axis and y axis labels
  scale_x_continuous(labels = NULL, breaks = NULL)+
  scale_y_continuous(labels = NULL, breaks = NULL)+
  #Add different colored dots to minimum points
  geom_point(data = binding_energy %>% filter(y == min(y)), color = "green") +
  geom_point(data = binding_energy %>% filter(x < -1.5) %>% filter(y == min(y)), color = "red") +
  geom_point(data = binding_energy %>% filter(x > 1.5) %>% filter(y == min(y)), color = "orange") +
  theme_light()
pes_plot
ggsave("pes_plot_1.png", plot = pes_plot, width = 6, height = 4, units = "in", dpi = 300)

pes_plot2 <- ggplot(binding_energy, aes(x = x, y = y2)) +
  geom_line() +
  labs(x = "Generalized Coordinates of the System",
       y = "Binding Energy Estimate (Score)") +
  #remove x axis and y axis labels
  scale_x_continuous(labels = NULL, breaks = NULL)+
  scale_y_continuous(labels = NULL, breaks = NULL)+
  #Add different colored dots to minimum points
  geom_point(data = binding_energy %>% filter(y == min(y)), color = "green") +
  geom_point(data = binding_energy %>% filter(x < 0) %>% filter(y2 == min(y2)), color = "red") +
  geom_point(data = binding_energy %>% filter(x > 0) %>% filter(y2 == min(y2)), color = "orange") +
  theme_light()
pes_plot2
ggsave("pes_plot_2.png", plot = pes_plot2, width = 6, height = 4, units = "in", dpi = 300)

pes_plot3 <- ggplot(binding_energy, aes(x = x, y = y3)) +
  geom_line() +
  labs(x = "Generalized Coordinates of the System",
       y = "Binding Energy Estimate (Score)") +
  #remove x axis and y axis labels
  scale_x_continuous(labels = NULL, breaks = NULL)+
  scale_y_continuous(labels = NULL, breaks = NULL)+
  #Add different colored dots to minimum points
  geom_point(data = binding_energy %>% filter(y == min(y)), color = "green") +
  geom_point(data = binding_energy %>% filter(x < 0) %>% filter(y2 == min(y2)), color = "red") +
  geom_point(data = binding_energy %>% filter(x > 0) %>% filter(y2 == min(y2)), color = "orange") +
  theme_light()
pes_plot3
ggsave("pes_plot_3.png", plot = pes_plot2, width = 6, height = 4, units = "in", dpi = 300)


#Now generate plot 1 but adding a legend where green dot = "Experimental structure and binding energy", red dot = "Local minimum 1", orange dot = "Local minimum 2"
pes_plotB <- pes_plot +
  #Label the different colored dots with arrows using geom_repel
    geom_label_repel(data = binding_energy %>% filter(y == min(y)), aes(label = "Experimental structure and binding energy"), label.size=1.1 , box.padding = 0.5, point.padding = 0.5, color = "green", segment.size = 0.5,nudge_x = 0.05, nudge_y = -0.1) +
    geom_label_repel(data = binding_energy %>% filter(y == min(y)), aes(label = "Experimental structure and binding energy"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = NA , nudge_x = 0.05, nudge_y = -0.1) +
    geom_label_repel(data = binding_energy %>% filter(x < -1.5) %>% filter(y == min(y)), aes(label = "Local minimum 1"), label.size=1.1, box.padding = 0.5, point.padding = 0.5, color = "red", segment.size = 0.5, nudge_x = 0.05, nudge_y = 0.1) +
    geom_label_repel(data = binding_energy %>% filter(x < -1.5) %>% filter(y == min(y)), aes(label = "Local minimum 1"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black", segment.color = NA , nudge_x = 0.05, nudge_y = 0.1) +
    geom_label_repel(data = binding_energy %>% filter(x > 1.5) %>% filter(y == min(y)), aes(label = "Local minimum 2"), label.size=1.1, box.padding = 0.5, point.padding = 0.5, color = "orange", segment.size = 0.5, nudge_x = 0.05, nudge_y = 0.1) +
    geom_label_repel(data = binding_energy %>% filter(x > 1.5) %>% filter(y == min(y)), aes(label = "Local minimum 2"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black", segment.color = NA , nudge_x = 0.05, nudge_y = 0.1) +
  theme_light()
pes_plotB
ggsave("pes_plot_1B.png", plot = pes_plotB, width = 6, height = 4, units = "in", dpi = 300)


#Now generate plot 1 but adding a legend where green dot = "Experimental structure and binding energy", red dot = "Local minimum 1", orange dot = "Local minimum 2"
pes_plotC <- pes_plot3 +
  #Label the different colored dots with arrows using geom_repel
    geom_label_repel(data = binding_energy %>% filter(x==0), aes(label = "Experimental structure and binding energy"), label.size=1.1 , box.padding = 0.5, point.padding = 0.5, color = "green", segment.size = 0.5,nudge_x = 0.05, nudge_y = 0.1) +
    geom_label_repel(data = binding_energy %>% filter(x==0), aes(label = "Experimental structure and binding energy"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = NA , nudge_x = 0.05, nudge_y = 0.1) +
    geom_label_repel(data = binding_energy %>% filter(x == -2) %>% filter(y3 == min(y3)), aes(label = "Local minimum 1"), label.size=1.1, box.padding = 0.5, point.padding = 0.5, color = "red", segment.size = 0.5, nudge_x = 0.05, nudge_y = 0.1) +
    geom_label_repel(data = binding_energy %>% filter(x == -2) %>% filter(y3 == min(y3)), aes(label = "Local minimum 1"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black", segment.color = NA , nudge_x = 0.05, nudge_y = 0.1) +
    geom_label_repel(data = binding_energy %>% filter(x == 2) %>% filter(y3 == min(y3)), aes(label = "Local minimum 2"), label.size=1.1, box.padding = 0.5, point.padding = 0.5, color = "orange", segment.size = 0.5, nudge_x = 0.05, nudge_y = 0.1) +
    geom_label_repel(data = binding_energy %>% filter(x == 2) %>% filter(y3 == min(y3)), aes(label = "Local minimum 2"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black", segment.color = NA , nudge_x = 0.05, nudge_y = 0.1) +
  theme_light()
pes_plotC
ggsave("pes_plot_1C.png", plot = pes_plotC, width = 6, height = 4, units = "in", dpi = 300)


pes_plotC <- pes_plot2 +
  #Label the different colored dots with arrows using geom_repel
    geom_label_repel(data = binding_energy %>% filter(x==0), aes(label = "Experimental structure and binding energy"), label.size=1.1 , box.padding = 0.5, point.padding = 0.5, color = "green", segment.size = 0.5,nudge_x = 0.00, nudge_y = 0.3) +
    geom_label_repel(data = binding_energy %>% filter(x==0), aes(label = "Experimental structure and binding energy"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = NA , nudge_x = 0.00, nudge_y = 0.3) +
    geom_label_repel(data = binding_energy %>% filter(x < 0) %>% filter(y2 == min(y2)), aes(label = "Local minimum 1"), label.size=1.1, box.padding = 0.5, point.padding = 0.5, color = "red", segment.size = 0.5, nudge_x = 0.00, nudge_y = -0.1) +
    geom_label_repel(data = binding_energy %>% filter(x < 0) %>% filter(y2 == min(y2)), aes(label = "Local minimum 1"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black", segment.color = NA , nudge_x = 0.00, nudge_y = -0.1) +
    geom_label_repel(data = binding_energy %>% filter(x > 0) %>% filter(y2 == min(y2)), aes(label = "Local minimum 2"), label.size=1.1, box.padding = 0.5, point.padding = 0.5, color = "orange", segment.size = 0.5, nudge_x = 0.00, nudge_y = -0.1) +
    geom_label_repel(data = binding_energy %>% filter(x > 0) %>% filter(y2 == min(y2)), aes(label = "Local minimum 2"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black", segment.color = NA , nudge_x = 0.00, nudge_y = -0.1) +
  theme_light()
pes_plotC
ggsave("pes_plot_2B.png", plot = pes_plotC, width = 6, height = 4, units = "in", dpi = 300)


#Now take binding energy, create a new column named surface with value 1, duplicate all rows with surface value 2 and 3
binding_energy <- binding_energy_ %>% mutate(surface = 1)
binding_energy2 <- binding_energy_ %>% mutate(surface = 2)
binding_energy3 <- binding_energy_ %>% mutate(surface = 3)
binding_energy <- bind_rows(binding_energy, binding_energy2, binding_energy3)


#Modify the y values for surface 2, adding three different gaussians but maintaining the y value for x=0
binding_energy$y[binding_energy$surface == 2] <- with(binding_energy[binding_energy$surface == 1,], 0.0 * exp(-(x-0)^2) - 3.0 * exp(-(x - 1)^2) - 3 * exp(-(x + 1)^2))
#Do the same for surface 3, with different gaussians but same y value for x=0 as surface 1
binding_energy$y[binding_energy$surface == 3] <- with(binding_energy[binding_energy$surface == 1,], -1.0 * exp(-(x+0.3)^2) - 1.5 * exp(-(x - 2)^2) - 0.5 * exp(-(x + 2)^2))

#Save value of y for x=0 for each surface
y0_surface1 <- binding_energy[binding_energy$surface == 1 & binding_energy$x == 0, "y"]
y0_surface2 <- binding_energy[binding_energy$surface == 2 & binding_energy$x == 0, "y"]
y0_surface3 <- binding_energy[binding_energy$surface == 3 & binding_energy$x == 0, "y"]

#Modify the y values for surface 2 and 3 to have the same y value for x=0,x=5 and x=-5 as surface 1
binding_energy$y[binding_energy$surface == 2] <- binding_energy$y[binding_energy$surface == 2] * y0_surface1 / y0_surface2
binding_energy$y[binding_energy$surface == 3] <- binding_energy$y[binding_energy$surface == 3] * y0_surface1 / y0_surface3



#Create a plot of the potential energy surface with different colored regions for different surfaces
pes_plot4 <- ggplot(binding_energy, aes(x = x, y = y, color = as.factor(surface))) +
  geom_line() +
  labs(x = "Generalized Coordinates of a Protein-ligand complex",
       y = "Binding Energy Estimate (Score)") +
#Add 
  scale_color_manual(values = c("#e69f00", "#56b4e9", "#009E73")) +
  scale_x_continuous(labels = NULL, breaks = NULL)+
  scale_y_continuous(labels = NULL, breaks = NULL)+
  #add a dot for x=0
    geom_point(data = binding_energy %>% filter(surface==1 & x == 0) %>% mutate(y= -1.01), color = "black") +
  #add a black dotted line for y=  -0.05
    geom_hline(yintercept = -1.01, linetype = "dotted", color = "black") +
  #add label for dot
    geom_label_repel(data = binding_energy %>% filter(surface==1 & x == 0) %>% mutate(y= -1.01), aes(label = "Experimental\nStructure"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = "black" , nudge_x = 0.00, nudge_y = -0.6) +
    geom_label_repel(data = binding_energy %>% filter(surface==1 & x == 0) %>% mutate(y= -1.01), aes(label = "Experimental\nBinding affinity"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = NA , nudge_x = -2.90, nudge_y = 0.0) +
  #add visible non overlapping labels for lines
    geom_label_repel(data = binding_energy %>% filter(surface==1 & x == -1) , aes(label = "Scoring function 1"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "#e69f00",  segment.color = NA , nudge_x = 0.50, nudge_y = 0.15) +
    geom_label_repel(data = binding_energy %>% filter(surface==2 & x == 2.0) , aes(label = "Scoring function 2"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "#56b4e9",  segment.color = NA , nudge_x = -0.80, nudge_y = 0.4) +
    geom_label_repel(data = binding_energy %>% filter(surface==3 & x == 2.5) , aes(label = "Scoring function 3"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "#009E73",  segment.color = NA , nudge_x = 0.05, nudge_y = 0.0) +
  theme_light()+
  #remove legend
    theme(legend.position = "none")
  pes_plot4
ggsave("pes_plot_2_colors.png", plot = pes_plot4, width = 6, height = 4, units = "in", dpi = 300)
#cbPalette <- c("#d55e00", "#009E73", "#F5C710")
#scale_color_manual(values = c("0 dose" = "#000000", "1+ doses" = "#e69f00", "2+ doses" = "#56b4e9", "3+ doses" = "#0071b2"))

#Do the correct virtual screening plot
binding_energy <- data.frame(x = pes$x, y = -pes$y, y2= -pes$y2, y3= -pes$y3)
#Now take binding energy, create a new column named surface with value 1, duplicate all rows with surface value 2 and 3
binding_energy <- binding_energy %>% mutate(surface = 1)
binding_energy <- binding_energy %>% bind_rows(mutate(binding_energy, surface = 2))
binding_energy <- binding_energy %>% bind_rows(mutate(binding_energy, surface = 3))
binding_energy <- binding_energy %>% bind_rows(mutate(binding_energy, surface = 4))


binding_energy$y[binding_energy$surface == 1] <- with(binding_energy[binding_energy$surface == 1,], -3.0 * exp(-(x+0.3)^2) - 1.0 * exp(-(x - 2.5)^2) - 0.3 * exp(-(x + 2)^2))
#Modify the y values for surface 2, adding three different gaussians but maintaining the y value for x=0
binding_energy$y[binding_energy$surface == 2] <- with(binding_energy[binding_energy$surface == 1,], -1.0 * exp(-(x-0)^2) - 0.5 * exp(-(x - 1)^2) - 1.0 * exp(-(x + 2.5)^2) - 1 * exp(-(x + 2)^2)  - 1 * exp(-(x + 1)^2))
#Do the same for surface 3, with different gaussians but same y value for x=0 as surface 1
binding_energy$y[binding_energy$surface == 3] <- with(binding_energy[binding_energy$surface == 1,], -1.0 * exp(-(x+0.3)^2) - 1.5 * exp(-(x - 2)^2) - 0.5 * exp(-(x + 2)^2))
#Surface 4
binding_energy$y[binding_energy$surface == 4] <- with(binding_energy[binding_energy$surface == 1,], -0.5 * exp(-(x-0)^2) - 2.0 * exp(-(x - 1)^2) - 1 * exp(-(x + 1)^2))

#Create a plot of the potential energy surface with different colored regions for different surfaces
pes_plot5 <- ggplot(binding_energy, aes(x = x, y = y, color = as.factor(surface))) +
  geom_line() +
  labs(x = "Generalized Coordinates of the System",
       y = "Binding Energy Estimate (Score)") +
  scale_color_manual(values = c("green", "gray", "gray", "gray")) +
  scale_x_continuous(labels = NULL, breaks = NULL)+
  scale_y_continuous(labels = NULL, breaks = NULL)+
  #add a dot for x=0
    geom_point(data = binding_energy %>% filter(surface==1) %>% filter(y == min(y)) %>% unique(), color = "black") +
    geom_label_repel(data = binding_energy %>% filter(surface==1) %>% filter(y == min(y)) %>% unique(), aes(label = "Active compound"), label.size=1.1 , box.padding = 0.5, point.padding = 0.5, color = "green", segment.size = 0.5,nudge_x = 0.00, nudge_y = -0.3) +
    geom_label_repel(data = binding_energy %>% filter(surface==1) %>% filter(y == min(y)) %>% unique(), aes(label = "Active compound"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = NA , nudge_x = 0.00, nudge_y = -0.3) +
    geom_point(data = binding_energy %>% filter(surface==2) %>% filter(y == min(y)) %>% unique(), color = "red") +
    geom_label_repel(data = binding_energy %>% filter(surface==2) %>% filter(y == min(y)) %>% unique(), aes(label = "Decoy1"), label.size=1.1 , box.padding = 0.5, point.padding = 0.5, color = "red", segment.size = 0.5,nudge_x = -0.30, nudge_y = -0.0) +
    geom_label_repel(data = binding_energy %>% filter(surface==2) %>% filter(y == min(y)) %>% unique(), aes(label = "Decoy1"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = NA , nudge_x = -0.30, nudge_y = -0.0) +
    geom_point(data = binding_energy %>% filter(surface==3) %>% filter(y == min(y)) %>% unique(), color = "red") +
        geom_label_repel(data = binding_energy %>% filter(surface==3) %>% filter(y == min(y)) %>% unique(), aes(label = "Decoy2"), label.size=1.1 , box.padding = 0.5, point.padding = 0.5, color = "red", segment.size = 0.5,nudge_x = 0.20, nudge_y = -0.0) +
    geom_label_repel(data = binding_energy %>% filter(surface==3) %>% filter(y == min(y)) %>% unique(), aes(label = "Decoy2"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = NA , nudge_x = 0.20, nudge_y = -0.0) +
    geom_point(data = binding_energy %>% filter(surface==4) %>% filter(y == min(y)) %>% unique(), color = "red") +
        geom_label_repel(data = binding_energy %>% filter(surface==4) %>% filter(y == min(y)) %>% unique(), aes(label = "Decoy3"), label.size=1.1 , box.padding = 0.5, point.padding = 0.5, color = "red", segment.size = 0.5,nudge_x = 0.30, nudge_y = 0.0) +
    geom_label_repel(data = binding_energy %>% filter(surface==4) %>% filter(y == min(y)) %>% unique(), aes(label = "Decoy3"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = NA , nudge_x = 0.30, nudge_y = 0.0) +
  theme_light()+
  #remove legend
    theme(legend.position = "none")

  pes_plot5
ggsave("pes_plot_VS.png", plot = pes_plot5, width = 6, height = 4, units = "in", dpi = 300)

#get x values for minimums
s1min1 <- binding_energy %>% filter(surface==1) %>% filter(y == min(y)) %>% unique()
s2min1 <- binding_energy %>% filter(surface==2) %>% filter(y == min(y)) %>% unique()
s3min1 <- binding_energy %>% filter(surface==3) %>% filter(y == min(y)) %>% unique()
s4min1 <- binding_energy %>% filter(surface==4) %>% filter(y == min(y)) %>% unique()


#Do the correct virtual screening plot
binding_energy <- data.frame(x = pes$x, y = -pes$y, y2= -pes$y2, y3= -pes$y3)
#Now take binding energy, create a new column named surface with value 1, duplicate all rows with surface value 2 and 3
binding_energy <- binding_energy %>% mutate(surface = 1)
binding_energy <- binding_energy %>% bind_rows(mutate(binding_energy, surface = 2))
binding_energy <- binding_energy %>% bind_rows(mutate(binding_energy, surface = 3))
binding_energy <- binding_energy %>% bind_rows(mutate(binding_energy, surface = 4))


binding_energy$y[binding_energy$surface == 1] <- with(binding_energy[binding_energy$surface == 1,], -0.9 * exp(-(x+0.3)^2) - 1.0 * exp(-(x - 2.5)^2) - 0.6 * exp(-(x + 2)^2))
#Modify the y values for surface 2, adding three different gaussians but maintaining the y value for x=0
binding_energy$y[binding_energy$surface == 2] <- with(binding_energy[binding_energy$surface == 1,], -0.5 * exp(-(x-0)^2) - 1.5 * exp(-(x - 1)^2) - 0.5 * exp(-(x + 1.5)^2)  - 0.75 * exp(-(x + 1)^2))
#Do the same for surface 3, with different gaussians but same y value for x=0 as surface 1
binding_energy$y[binding_energy$surface == 3] <- with(binding_energy[binding_energy$surface == 1,], -1.0 * exp(-(x+0.3)^2) - 0.0 * exp(-(x - 2)^2) - 1.5 * exp(-(x + 2)^2))
#Surface 4
binding_energy$y[binding_energy$surface == 4] <- with(binding_energy[binding_energy$surface == 1,], -0.25 * exp(-(x-0)^2) - 0.3 * exp(-(x - 1)^2) - 1.1 * exp(-(x + 1)^2))

#Create a plot of the potential energy surface with different colored regions for different surfaces
pes_plot6 <- ggplot(binding_energy, aes(x = x, y = y, color = as.factor(surface))) +
  geom_line() +
  labs(x = "Generalized Coordinates of the System",
       y = "Binding Energy Estimate (Score)") +
  scale_color_manual(values = c("green", "gray", "gray", "gray")) +
  scale_x_continuous(labels = NULL, breaks = NULL)+
  scale_y_continuous(labels = NULL, breaks = NULL)+
  #add a dot for x=0
    geom_point(data = binding_energy %>% filter(surface==1) %>% filter(x %in% s1min1$x ) %>% unique(), color = "black") +
    geom_label_repel(data = binding_energy %>% filter(surface==1) %>% filter(x %in% s1min1$x ) %>% unique(), aes(label = "Active compound"), label.size=1.1 , box.padding = 0.5, point.padding = 0.5, color = "green", segment.size = 0.5,nudge_x = 0.00, nudge_y = -0.3) +
    geom_label_repel(data = binding_energy %>% filter(surface==1) %>% filter(x %in% s1min1$x ) %>% unique(), aes(label = "Active compound"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = NA , nudge_x = 0.00, nudge_y = -0.3) +
    geom_point(data = binding_energy %>% filter(surface==2) %>% filter(x %in% s2min1$x ) %>% unique(), color = "red") +
    geom_label_repel(data = binding_energy %>% filter(surface==2) %>% filter(x %in% s2min1$x ) %>% unique(), aes(label = "Decoy1"), label.size=1.1 , box.padding = 0.5, point.padding = 0.5, color = "red", segment.size = 0.5,nudge_x = -0.30, nudge_y = -0.0) +
    geom_label_repel(data = binding_energy %>% filter(surface==2) %>% filter(x %in% s2min1$x ) %>% unique(), aes(label = "Decoy1"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = NA , nudge_x = -0.30, nudge_y = -0.0) +
    geom_point(data = binding_energy %>% filter(surface==3) %>% filter(x %in% s3min1$x ) %>% unique(), color = "red") +
        geom_label_repel(data = binding_energy %>% filter(surface==3) %>% filter(x %in% s3min1$x ) %>% unique(), aes(label = "Decoy2"), label.size=1.1 , box.padding = 0.5, point.padding = 0.5, color = "red", segment.size = 0.5,nudge_x = 0.20, nudge_y = -0.0) +
    geom_label_repel(data = binding_energy %>% filter(surface==3) %>% filter(x %in% s3min1$x ) %>% unique(), aes(label = "Decoy2"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = NA , nudge_x = 0.20, nudge_y = -0.0) +
    geom_point(data = binding_energy %>% filter(surface==4) %>% filter(x %in% s4min1$x ) %>% unique(), color = "red") +
        geom_label_repel(data = binding_energy %>% filter(surface==4) %>% filter(x %in% s4min1$x ) %>% unique(), aes(label = "Decoy3"), label.size=1.1 , box.padding = 0.5, point.padding = 0.5, color = "red", segment.size = 0.5,nudge_x = 0.30, nudge_y = 0.0) +
    geom_label_repel(data = binding_energy %>% filter(surface==4) %>% filter(x %in% s4min1$x ) %>% unique(), aes(label = "Decoy3"), label.size=NA , box.padding = 0.5, point.padding = 0.5, color = "black",  segment.color = NA , nudge_x = 0.30, nudge_y = 0.0) +
  theme_light()+
  #remove legend
    theme(legend.position = "none")

  pes_plot6
ggsave("pes_plot_VS2.png", plot = pes_plot6, width = 6, height = 4, units = "in", dpi = 300)



#Create a dataframe where y=ax+b, where a=0.6 and b=-8, and add some noise
data <- data.frame(x = seq(-12, -2, length.out = 201))
data$y <- 0.35 * data$x - 4.75 + rnorm(201, sd = 1.75)
#add an rnorm error term wich depends on x
data$y <- data$y + rnorm(201, sd = abs(0.25 / data$x))
data$y2 <- 0.7 * data$x - 2.25 + rnorm(201, sd = 1.2)
data$y2 <- data$y2 + rnorm(201, sd = abs(0.1 / data$x))

mean(data$x)
mean(data$y)
mean(data$y2)


#Wide data to long data
data <- data %>% gather(key = "surface", value = "y", y:y2)

#Measure pearson correlation for each surface
correlations <- data %>% 
  group_by(surface) %>% 
  summarise(correlation = cor(x, y, method = "pearson"))

# Create a plot of the data

# Define the labels for each level of the surface variable
labels = paste(correlations$surface, " (Correlation: ", round(correlations$correlation, 2), ")")


# Create a plot of the data
data_plot <- ggplot(data, aes(x = x, y = y, shape = surface)) +
  geom_point(size = 2, stroke = 2) +
  scale_shape_manual(values = c(21, 19), labels = labels) +  # 21 is filled circle, 19 is open circle
  labs(x = "Experimental Binding Energy",
       y = "Predicted Binding Energy (Score)",
       shape = "Scoring Function") +  # Change the shape legend title
  #add a line for y=x
  geom_abline(intercept = 0, slope = 1, color = "red") +
  scale_x_continuous(limits=c(-12,-2))+
  scale_y_continuous(limits=c(-12,-2))+
  #legend below
  theme_light()+
  theme(legend.position = "bottom")
data_plot

ggsave("scatter_plot.png", plot = data_plot, width = 6, height = 6, units = "in", dpi = 300)




