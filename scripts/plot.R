# The figure uses all the data which is too large so make
# an image to include in the paper
p1 <- ggplot(data = student_country,
             aes(x = math, y = read)) +
  geom_hex() +
  labs(x = "math",
       y = "reading") +
  theme_bw() +
  theme(aspect.ratio=1, legend.position="none")

p2 <- ggplot(data = student_country,
             aes(x = math, y = science)) +
  geom_hex() +
  labs(x = "math",
       y = "science") +
  theme_bw() +
  theme(aspect.ratio=1, legend.position = "none")

p3 <- ggplot(data = student_country,
             aes(x = read, y = science)) +
  geom_hex() +
  labs(x = "reading",
       y = "science") +
  theme_bw() +
  theme(aspect.ratio=1, legend.position="none")

p1+p2+p3
ggsave("figures/hexbin.png")

# This code can be used to generate an animation like
# the gapminder animation but it doesn't yield any
# consistent improvement in test scores over the years
# unlike the consistent improvement in health and
# wealth seen in gapminder.
load("data/student_anim_data.rda")
gif <- ggplot(student_anim_data,
              aes(x=math_avg, y=read_avg,
                  color = continent, group = country_name)) +
  geom_point(size=2, alpha=0.5) +
  geom_label(data = filter(student_anim_data,
                           country_name %in%
                             c("Australia",
                               "New Zealand",
                               "Indonesia",
                               "Qatar",
                               "Singapore",
                               "Germany",
                               "Malaysia",
                               "Finland",
                               "Canada",
                               "Thailand",
                               "Brazil",
                               "Colombia",
                               "Chile",
                               "USA")),
             aes(label = country_name,
                 group = country_name), size=4) +
  theme_minimal() +
  theme(legend.position = "none",
        #axis.line = element_blank(),
        aspect.ratio=1) +
  transition_states(year,
                    transition_length = 1,
                    state_length = 1,
                    wrap = FALSE)   +
  scale_colour_brewer("", palette = "Dark2") +
  labs(title = 'Year: {closest_state}',
       x = "Math",
       y = "Reading") +
  xlim(c(250, 650)) + ylim(c(300, 600)) +
  enter_fade() + exit_fade()

animate(gif, fps = 5, end_pause = 1)
