#Create plots of potential energy surface, binding energy on y axis versus atomic coordinates on x axis
library(ggplot2)
library(dplyr)
library(ggrepel)

#Create a data frame with the potential energy surface, which corresponds to the sum of three gaussians
pes <- data.frame(x = seq(-5, 5, length.out = 1001))
pes$y <- with(pes, 1.0 * exp(-(x-0)^2) + 0.5 * exp(-(x - 2)^2) + 0.4 * exp(-(x + 2)^2))

#Create a data frame with the binding energy, which is the negative of the potential energy surface
binding_energy <- data.frame(x = pes$x, y = -pes$y)

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


#Now take binding energy, create a new column named surface with value 1, duplicate all rows with surface value 2 and 3
binding_energy <- binding_energy %>% mutate(surface = 1)
binding_energy <- binding_energy %>% bind_rows(mutate(binding_energy, surface = 2))
binding_energy <- binding_energy %>% bind_rows(mutate(binding_energy, surface = 3))

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
pes_plot2 <- ggplot(binding_energy, aes(x = x, y = y, color = as.factor(surface))) +
  geom_line() +
  labs(x = "Generalized Coordinates of the System",
       y = "Binding Energy Estimate (Score)") +
  scale_color_manual(values = c("blue", "green", "red")) +
  scale_x_continuous(labels = NULL, breaks = NULL)+
  scale_y_continuous(labels = NULL, breaks = NULL)+
  #add a dot for x=0
    geom_point(data = binding_energy %>% filter(x == 0), color = "black") +
  theme_light()+
  #remove legend
    theme(legend.position = "none")

  pes_plot2
ggsave("pes_plot_2.png", plot = pes_plot2, width = 6, height = 4, units = "in", dpi = 300)


#Create a dataframe where y=ax+b, where a=0.6 and b=-8, and add some noise
data <- data.frame(x = seq(-12, -2, length.out = 301))
data$y <- 0.35 * data$x - 4.75 + rnorm(301, sd = 1.75)
#add an rnorm error term wich depends on x
data$y <- data$y + rnorm(301, sd = abs(0.25 / data$x))
data$y2 <- 0.7 * data$x - 2.25 + rnorm(301, sd = 1.2)
data$y2 <- data$y2 + rnorm(301, sd = abs(0.1 / data$x))

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
data_plot <- ggplot(data, aes(x = x, y = y, color = surface)) +
  geom_point() +
  labs(x = "Experimental Binding Energy",
       y = "Predicted Binding Energy (Score)",
       color = "Scoring Function") +  # Change the color legend title
  #add a line for y=x
  geom_abline(intercept = 0, slope = 1, color = "red") +
  scale_x_continuous(limits=c(-12,-2))+
  scale_y_continuous(limits=c(-12,-2))+
  scale_color_discrete(labels = paste(correlations$surface, " (Correlation: ", round(correlations$correlation, 2), ")")) +
  #legend below
  theme_light()+
  theme(legend.position = "bottom")
data_plot

ggsave("scatter_plot.png", plot = data_plot, width = 6, height = 6, units = "in", dpi = 300)