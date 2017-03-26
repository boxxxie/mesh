# mesh

Clojure library for importing csv and tsv files.

Automatically converts csv/tsv fields into standard Clojure data types.

## Lein

Add the following dependency to your project.clj file:

``` clj
[mesh "0.0.2"]

```

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
