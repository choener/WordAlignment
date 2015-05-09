[![Build Status](https://travis-ci.org/choener/WordAlignment.svg?branch=master)](https://travis-ci.org/choener/WordAlignment)

# Word alignments in natural languages

This library and program are designed for the alignment of *words* in human
languages. Each word is encoded in a particular way (as the mapping between
characters and their /meaning/ (?) is not well-defined enough).

As user, you send a list to stdin that is formatted as follows (with all fields
mandatory):

word id
language name
meaning identifier (this is a string, not a number)
length of the word
space-separated characters (each character is actually a string)

0	Albanian_Tosk	1.100	5	\' b o t ə
2	Albanian_Tosk	1.100	10	r̃ o k u lʸ i a\' lʸ e m


In addition, a score file need to be given (--scorefile) with

language name
character (bigram 1.1)
character (bigram 1.2)
language name
character (bigram 2.1)
character (bigram 2.2)
score

Albanian_Tosk \' a Albanian_Tosk \' a 4.25238
Albanian_Tosk \' a Albanian_Tosk \' b 0.402228
Albanian_Tosk \' a Albanian_Tosk \' g 1.07432


Some additional pieces of evidence are required. First is the default score
given whenever a pair of bigrams can not be matched against in the score file.
This defualt score (--defaultscore) defaults to (-42) due to popular
requirement. If the first line in the score file contains just a single double
value, that value is taken instead. Second, the cost to open a gap (--gapopen)
needs to be given. It defaults to (0) which is probably to high, as now it is
mostly better to just score two in/dels instead of a slightly worse match.

Once this data is available, start the program. Here, we restrict ourselves to
the albanian language (albanian.input), from which a only take the three first
words. Then we call the WordAlign program, asking for twoway alignments
(twoway), using just the scores for the albanian language (-s albanian.scores).
We set the gapopen cost to (-2) with (-g -2), and if you need program runtime
statistics, you may add (+RTS -s -RTS), but that is not required.

The output are four lines for each alignment. An info line with the word ids
(IDS), the alignment score (SCORE), the normalized scores (NSCORE) and the
actual words, started by (WORDS) and interleaved by (WORD). The next two lines
are the alignment, with deletions showning up as minus symbols (---) in the
deletion field. Note that a deletion does not delete a character from the
input, it merely aligns an existing character in one alignment with the symbol
for deletion (--) in the other.  After the alignment follows one empty line.

The normalized score is defined as SCORE / maximum of input word lengths

cat albanian.input | head -n 3 | ./dist/build/WordAlign/WordAlign twoway -s albanian.scores

IDS: 0 1 SCORE: -1.28 NSCORE: -0.18    WORDS: ^ \' b o t ə $   WORD   ^ ð e $
 ^ \' b o t ə $
 ^ -- - ð e - $

IDS: 0 2 SCORE: 1.46 NSCORE: 0.12    WORDS: ^ \' b o t ə $   WORD   ^ r̃ o k u lʸ i a\' lʸ e m $
 ^ -- - \' -  b -   o  t ə $ -
 ^ r̃ o  k u lʸ i a\' lʸ e m $

IDS: 1 2 SCORE: -1.56 NSCORE: -0.13    WORDS: ^ ð e $   WORD   ^ r̃ o k u lʸ i a\' lʸ e m $
 ^ -- - ð - -- -   e -- $ - -
 ^ r̃ o k u lʸ i a\' lʸ e m $



In addition, a simplified scoring model is available. Here, the word delimiters
(^) and ($) are not required, as the scoring model does not score the initial
and last character differently. The simplified scoring model consists of:
consonant equality, both consonant, vowel equality, both vowel, both of type
"other", and vowel vs. consonant. With scores of 3,1,1,0,0,-1 by default. Gap
costs are set via --gapopen with -1 by default.

 cat albanian.input | head -n 3 | ./dist/build/WordAlign/WordAlign twowaysimple
IDS: 0 1 SCORE: -3.00 NSCORE: -0.60    WORDS: \' b o t ə   WORD   ð e
 \' b o t ə
  ð - e - -

IDS: 0 2 SCORE: -3.00 NSCORE: -0.30    WORDS: \' b o t ə   WORD   r̃ o k u lʸ i a\' lʸ e m
 \' - b o  t ə --- -- - -
 r̃ o k u lʸ i a\' lʸ e m

IDS: 1 2 SCORE: -7.00 NSCORE: -0.70    WORDS: ð e   WORD   r̃ o k u lʸ i a\' lʸ e m
  ð - - - -- - --- -- e -
 r̃ o k u lʸ i a\' lʸ e m







## Some Notes

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



#### Contact

Christian Hoener zu Siederdissen
choener@bioinf.uni-leipzig.de
Leipzig University, Leipzig, Germany

