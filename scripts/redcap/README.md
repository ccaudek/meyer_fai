
Execute:

```r
library(targets)
tar_make()
```

This will create the target `fai_clean_complete` in the directory `_targets/objects`.

To read the data:

```r
# Read cleaned RedCap data
df <- tar_read(fai_clean_complete)
```
