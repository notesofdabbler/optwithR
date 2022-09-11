
#
# Map coloring problem with OMPR and HiGHS
#

library(dplyr)
library(glue)
library(ompr)
library(ompr.highs)
library(highs)
library(purrr)
library(ggplot2)
library(maps)
library(mapdata)

theme_set(theme_light())

# Processing state adjacency data in a format to use in OMPR model

#
# US states adjacency data from
# https://writeonly.wordpress.com/2009/03/20/adjacency-list-of-states-of-the-united-states-us/
#

x = readLines("data/US_state_adjacency.txt")

edgelist = list()
e = 0
for (i in 1:length(x)) {
  stloc = strsplit(x[i], ",")[[1]]
  ns = length(stloc)
  if(ns > 1) {
    for (i in 2:ns) {
      e = e + 1
      edgelist[[e]] = c(stloc[1], stloc[i])   
    }
  }
}

edge_df = as.data.frame(do.call(rbind, edgelist))
names(edge_df) = c("from", "to")

nodes_df = bind_rows(edge_df %>% distinct(from) %>% rename(state = from),
          edge_df %>% distinct(to) %>% rename(state = to)) %>% distinct(state)
nodes_df = nodes_df %>% mutate(id = seq(1, nrow(nodes_df)))

edge_df = inner_join(edge_df, nodes_df %>% rename(from = state), by = "from") %>% rename(fromid = id)
edge_df = inner_join(edge_df, nodes_df %>% rename(to = state), by = "to") %>% rename(toid = id)

edge_df = edge_df %>% filter(fromid < toid)

# OMPR model

ns = nrow(nodes_df)
nc = 4
edge_str = edge_df %>% mutate(edge_str = glue("{fromid}_{toid}")) %>% pull(edge_str)

mdl = MIPModel()
mdl = mdl %>% add_variable(x[i, c], i = 1:ns, c = 1:nc, type = "integer", lb = 0, ub = 1)
mdl = mdl %>% add_variable(y[c], c = 1:nc, type = "integer", lb = 0, ub = 1)
mdl = mdl %>% set_objective(sum_over(y[c], c=1:nc), sense = "min")
mdl = mdl %>% add_constraint(sum_over(x[i, c], c = 1:nc) == 1, i = 1:ns)
mdl = mdl %>% add_constraint(x[i, c] + x[j, c] <= y[c], i = 1:ns, j = 1:ns, c = 1:nc, glue("{i}_{j}") %in% edge_str)

# solve model
sol = mdl %>% solve_model(highs_optimizer())


# Get solution and visualize in a map
sol[["status"]]
sol[["objective_value"]]

soldf = sol %>% get_solution(x[i, c]) %>% filter(value == 1) %>%
  rename(id = i, colid = c)

soldf = inner_join(soldf, nodes_df, by = "id")

colmap = c("1" = "red", "2" = "blue", "3" = "green", "4" = "yellow")
soldf = soldf %>% mutate(stcol = colmap[colid])

# Visualize solution in a map
#
# https://jtr13.github.io/cc19/different-ways-of-plotting-u-s-map-in-r.html
#

state <- map_data("state")
state_name_abb = tibble(stname = tolower(state.name), stabb = state.abb)

state = inner_join(state, state_name_abb %>% rename(region = stname), by = "region")
state = inner_join(state, soldf %>% select(state, stcol) %>% rename(stabb = state), by = "stabb")

ggplot(data=state, aes(x=long, y=lat, fill=stcol, group=group)) + 
  geom_polygon(color = "white") + 
  guides(fill="none") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3)