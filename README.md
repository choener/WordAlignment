
Some Notes
==========

The scoring data is ~800 mbyte on disk. In memory this grows to ~4000 mbyte due
to data structure overhead.

Overhead can be reduced by selecting input from a restricted set of languages
in each run. There is no need to filter the scoring table, that is done
automatically!

Initial creation of all data structures takes some time (3-4 minutes, if all
languages are present). For batch runs (--batch batchsize,start) it is a very
good idea to select a batch size that is quite large. Selecting 1-10 million
elements for two-way alignments is good. A lot fewer for three-way and four-way
alignments.

Typical numbers for alignments / second:

two-way: 5.000 - 10.000 alignments / second, on a Core i5
