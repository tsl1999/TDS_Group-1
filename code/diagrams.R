install.packages("DiagrammeR")
DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = LR]
graph[overlap = true]
# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen]

data1 [label = 'BioBank', shape = folder, fillcolor = Beige]
data2 [label = 'Subset', shape = folder, fillcolor = Beige]
data3 [label = 'Subset2', shape = folder, fillcolor = Beige]
selection1 [label = 'Variable subseting and Renaming', fillcolor = green]
selection2 [label = 'Variable aggregation', fillcolor = red]
preanalysis1 [label = 'Table1', fillcolor = red]
preanalysis2 [label = 'Variables Correlation', fillcolor = red]
preanalysis3 [label = 'Logistic Regressions', fillcolor = red]

# edge definitions with the node IDs
data1  -> selection1 -> data2 -> selection2 -> data3 -> {preanalysis1 preanalysis2 preanalysis3}
}")
