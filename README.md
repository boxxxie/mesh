# mesh

Import for csv and tsv files 

## Lein

[![Clojars Project](https://img.shields.io/clojars/v/mesh.svg)]


## Require

``` clj
(ns new-project
  (:require [mesh.core] :as mesh))

```

## Usage


``` clj
(def file-path "/myfile.csv")

(mesh/import-file file-path)

```


## License

Copyright © 2017 rnghack@gmail.com

Distributed under the [Apache License, version 2](http://www.apache.org/licenses/).
