install.packages("DiagrammeR")
DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = LR]
graph[overlap = true]
# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen]

data1 [label = 'BioBank', shape = folder, fillcolor = Beige]
data2 [label = 'Subset', shape = folder, fillcolor = Beige]
data3 [label = 'SubsetNutrition', shape = folder, fillcolor = Beige]
selection1 [label = 'Variable extraction and recoding', fillcolor = green]
selection2 [label = 'Variable aggregation', fillcolor = green]
preanalysis1 [label = 'Preanalysis:
Table1
Variables correlation
Logistic regressions
Data distributions', fillcolor = green]

preanalysis2 [label = 'Preanalysis: 
Table1
Regression with outcomes
Distributions', fillcolor = red]

NutritionScore [label = 'Nutrition score construction', fillcolor = green]
Score [label = 'NutritionScore', shape = circle, fillcolor = Blue]
Smoking [label = 'Smoking', shape = circle, fillcolor = Blue]
SNIPS [label = 'GeneticInstruments', shape = circle, fillcolor = Blue]

GWAS1 [label = 'GWAS', shape = circle, fillcolor = Green]
GWAS2 [label = 'GWAS', shape = circle, fillcolor = Red]

# edge definitions with the node IDs
data1  -> selection1 -> data2 -> selection2 -> data3 -> {preanalysis1, NutritionScore}
NutritionScore -> Score -> {preanalysis2, GWAS2}
data2 -> Smoking -> GWAS1
{GWAS1, GWAS2} -> SNIPS
}")

