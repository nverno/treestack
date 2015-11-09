library(treemap)
library(data.table)

load("data/income.rda")

## Aggregation function of multiple variables
sir <- function(expense, income) sum(expense) / sum(income) * 100


treestack <- function(data, fun.aggregate="sum", byCount=FALSE, ...) {
    ## If more than one column is involved in aggregation,
    ## make a dummy index
    arg_count <- sum(sapply(formals(fun.aggregate), `==`, bquote())) %||% 0
    
    if (byCount)
        dat <- as.data.table(income)[, `:=`()]
    
}
##

## Some dummy variables to aggregate by: ALL, i, and index
dat <- as.data.table(df)[, `:=`(total = factor("ALL"), i = 1, index = 1:.N)][]
indexList <- c('total', 'gender', 'education', 'residence')  # order or aggregation

## Function to aggregate at each grouping level (SIR)
agg <- function(index, ...) {
    dots <- list(...)
    expense <- dots[["expense"]][index]
    income <- dots[["income"]][index]
    sum(expense) / sum(income) * 100
}

## Get treemap data
res <- treemap(dat, index=indexList, vSize='i', vColor='index',
               type="value", fun.aggregate = "agg",
               palette = 'RdYlBu',
               income=dat[["income"]],
               expense=dat[["expense"]])  # ... args get passed to fun.aggregate


treegraph(dat, indexList, directed=TRUE, show.labels = TRUE, vertex.layout.params = "reingold.tilford")

## The useful variables: level (corresponds to indexList), vSize (bar size), vColor(SIR)
## Create a label variable that is the value of the variable in indexList at each level
out <- res$tm
out$label <- out[cbind(1:nrow(out), out$level)]
out$label <- with(out, ifelse(level==4, substring(label, 1, 1), label))  # shorten labels
out$level <- factor(out$level, levels=sort(unique(out$level), TRUE))     # factor levels

## Time to find label positions, scale to [0, 1] first
## x-value is cumsum by group,  y will just be the level
out$xlab <- out$vSize / max(aggregate(vSize ~ level, data=out, sum)$vSize)
split(out$xlab, out$level) <- lapply(split(out$xlab, out$level), function(x) cumsum(x) - x/2)

## Make plot
library(ggplot2)
ggplot(out, aes(x=level, y=vSize, fill=color, group=interaction(level, label))) +
  geom_bar(stat='identity', position='fill') +  # add another for black rectangles but not legend
  geom_bar(stat='identity', position='fill', color="black", show_guide=FALSE) +
  geom_text(data=out, aes(x=level, y=xlab, label=label, ymin=0, ymax=1), size=6, font=2,
            inherit.aes=FALSE) +
  coord_flip() +
  scale_fill_discrete('SIR', breaks=out$color, labels = round(out$vColor)) +
  theme_minimal() +  # Then just some formatting 
  xlab("") + ylab("") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
