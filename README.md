# mesh

Import for csv and tsv files 

## Lein

Add the following dependency to your project.clj file:

[![Clojars Project](https://clojars.org/mesh/latest-version.svg)]

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
