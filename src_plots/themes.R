
theme_simedb <- function(){
  ggplot2::theme_light() +
    theme(legend.position = "bottom") +
    theme(plot.caption = element_text(size = 6, hjust = 0))
}

theme_simedb_no_x <- function(){
  theme_simedb() +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())
}

theme_simedb_no_y <- function(){
  theme_simedb() +
    theme(axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank())
}

theme_simedb_rotate_x <- function(){
  theme_simedb() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

theme_simedb_seigneurs <- function(x){
  theme_simedb_rotate_x() +
    theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
}

theme_simedb_map <- function(){
  theme_simedb() +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank()
    ) +
    theme(axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()
    ) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )
}