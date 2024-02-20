# clj-hunalgo

A Clojure library for solving the [assignment problem](https://en.wikipedia.org/wiki/Assignment_problem) using the [Hungarian algorithm](https://en.wikipedia.org/wiki/Hungarian_algorithm). This code is a port of the Java library found here:

https://github.com/aalmi/HungarianAlgorithm/blob/master/HungarianAlgorithm.java

## Usage

Call the function `solve-assignment-problem-ij-pairs` to find the optimum assignment represented as pairs of row and column indices:

```
(require '[clj-hunalgo.core :refer [solve-assignment-problem-ij-pairs]])

(solve-assignment-problem-ij-pairs
 [[1 0 1 1]
  [1 1 0 1]
  [1 1 1 0]
  [0 1 1 1]])
;; => ([0 1] [1 2] [2 3] [3 0])
```

## License

Copyright 2024 Jonas Östlund

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
