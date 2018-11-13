---
title: "Traversing economic activity hierarchies like NACE, SBI and SIC"
author: "Mark Zwart"
date: "2018-11-13"
output: 
  rmarkdown::html_vignette:
    css: graydon.css
    df_print: paged
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEncoding{UTF-8}
---

* [Introduction](#introduction)
    - [Example data](#example_data)
* [Creating a economic activity graph](#create_graph)
    - [Visualizing the hierarchy](#visualize)
* [Rolling up the hierarchy on values](#roll_up)




# <a name="introduction"></a>Introduction

Our company databases have codes that indicate a company's economic activity. Economic activities are indicated by the NACE, SBI or SIC codes, depending on the country. Although this classification in helpful when looking at a client's customer portfolio, it contains so many groups and in some cases so little customers per group, that it is hard to make a proper portfolio analysis to spot a clients opportunities. With this package you can make use of the hierarchical nature of the economic activity coding to make less and larger groups.


First let's load the package:

```r
library(graydon.package)
```

### <a name="example_data"></a>The example data

The package contains a data frame, _tbl_SBI_count_ which is an example of the SBI codes completed with a count of the Dutch companies associated with that SBI code. The data frame contains the following columns:

```r
data.frame(`Column names` = names(tbl_SBI_count)) %>% 
  knitr::kable()
```



|Column.names    |
|:---------------|
|code_SBI        |
|code_SBI_parent |
|description_SBI |
|hierarchy_layer |
|qty_companies   |

## <a name="create_graph"></a>Creating a economic activity graph

To do any calculation or plotting on the economic activity hierarchy, we have to create a graph. The parameters _col_id_ and _col_id_parent_ are the column names that contain the economic activity code and the parent code respectively.










