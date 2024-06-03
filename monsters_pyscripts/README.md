# Question Programs & Expected Information Gain - Monsters Task

This is codebase for parsing/executing questions and calculating Expected Information Gain (EIG) for question programs defined in the paper "Seeking new information with old questions: Children and adults reuse and remix concepts from prior questions."

This codebase is adapted from the [expected-information-gain](https://github.com/anselmrothe/EIG) package, by Anselm Rothe and Ziyun Wang. The original package is designed for the Battleship task, and we extend it to a new, child-friendly Monsters task.

Please see `eig_example.ipynb` for an example of how to represent questions as programs and calculate EIG for those programs.

Please see `similarity_example.ipynb` for an example of how to calculate the similarity between two questions/programs, using tree edit distance and semantic similarity.

The easiest way to start using this codebase is to create a new conda environment using the `environment.yml` file in this repository. This will install all the necessary dependencies.

The original EIG package was distributed under the MIT License, and we keep the same license for this codebase. Original license information below:

```
MIT License

Copyright (c) 2018 

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

