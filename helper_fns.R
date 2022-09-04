
# get nodes and edges data frame for visnetwork
make_visnetdf = function(df, from, to) {
  
  from_chr = deparse(substitute(from))
  to_chr = deparse(substitute(to))
  
  nodes_df = bind_rows(df %>% select({{from}}) %>% rename(node = {{from}}), 
                       df %>% select({{to}}) %>% rename(node = {{to}})) %>% 
    distinct(node)
  
  nodes_df = nodes_df %>% mutate(id = seq(1, nrow(nodes_df)))
  
  edges_df = inner_join(df, nodes_df %>% rename({{from}} := node, from = id), by = from_chr)
  edges_df = inner_join(edges_df, nodes_df %>% rename({{to}} := node, to = id), by = to_chr)
  
  return(list(nodes_df = nodes_df, edges_df = edges_df))
}