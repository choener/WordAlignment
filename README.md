[![Build Status](https://travis-ci.org/choener/WordAlignment.svg?branch=master)](https://travis-ci.org/choener/WordAlignment)



This manual can be displayed by calling just ```WordAlign``` or alternatively
```WordAlign manual```.



# Word alignments in natural languages

This library and program are designed for the alignment of *words* in human
languages.

Implemented with ideas described in:

1.  Christian Hoener zu Siederdissen  
    *Sneaking Around ConcatMap: Efficient Combinators for Dynamic Programming*  
    2012, Proceedings of the 17th ACM SIGPLAN international conference on Functional programming  
    [paper](http://doi.acm.org/10.1145/2364527.2364559) [preprint](http://www.tbi.univie.ac.at/newpapers/pdfs/TBI-p-2012-2.pdf)  
1.  Andrew Farmer, Christian Höner zu Siederdissen, and Andy Gill.  
    *The HERMIT in the stream: fusing stream fusion’s concatMap*  
    2014, Proceedings of the ACM SIGPLAN 2014 workshop on Partial evaluation and program manipulation.  
    [paper](http://dl.acm.org/citation.cfm?doid=2543728.2543736)  
1.  Christian Höner zu Siederdissen, Ivo L. Hofacker, and Peter F. Stadler.  
    *Product Grammars for Alignment and Folding*  
    2014, IEEE/ACM Transactions on Computational Biology and Bioinformatics. 99  
    [paper](http://ieeexplore.ieee.org/xpl/articleDetails.jsp?arnumber=6819790)  
1.  Christian Höner zu Siederdissen, Sonja J. Prohaska, and Peter F. Stadler  
    *Algebraic Dynamic Programming over General Data Structures*  
    2015, BMC Bioinformatics  
    [preprint](http://www.bioinf.uni-leipzig.de/Software/gADP/preprints/hoe-pro-2015.pdf)  



# Usage

Word alignments require three pieces of information: (i) the words to be
aligned, (ii) the selection of an alignment algorithm, and (iii) a scoring
scheme (either simple or bigram-based). Each of these is described below.



## Words to be Aligned

Each word is encoded as a list of *characters*. A single *character*, however,
is encoded as a *list of unicode symbols*, not just a single symbol.

This particular encoding is necessary because there is no general one-to-one
mapping of atomic alphabet symbols to unicode characters. Just consider
character decorations, such as *Umlaute* (even though Umlaute are available in
unicode).

As user, you send a list to stdin that is formatted as follows (with all fields
mandatory; the tab symbol being the separator between each column):

* word id
* language name
* meaning identifier (this is a string, not a number)
* length of the word
* space-separated characters (each character is actually a string)

(In case you read the plain-text version of this document, the 4 whitespace
characters should be left out.)

    0	Albanian_Tosk	1.100	5	\' b o t ə
    2	Albanian_Tosk	1.100	10	r̃ o k u lʸ i a\' lʸ e m



## Selection of Alignment Algorithm

The WordAlignment program comes with these modes:

| Grammar   | Simple Scoring  | Bigram Scoring  | Number of Input Tapes |
|-----------|-----------------|-----------------|-----------------------|
| Global    | global2simple   | global2bigram   | 2                     |
| Overhang  | global2infix    | global2infix    | 2                     |

Mode names can be shortened to unique prefixes.



### Global Alignments

Global alignments use a NeedlemanWunsch style algorithm with linear gap costs.



### Overhang alignments

Overhang alignments use affine gap scoring. Overhang alignments separate the
alignment into three phases, the prefix, infix, and suffix phase. Typically,
prefix and suffix phases have very low costs.



### Options

#### Common default options

    -? --help             Display help message
    -V --version          Print version information
       --numeric-version  Print just the version number
    -v --verbose          Loud verbosity
    -q --quiet            Quiet verbosity

```--verbose``` prints status information every 10.000 alignments



#### Common options for all alignments variants

       --simplescorefile=ITEM  the file to read the simple scores from
    -l --lpblock=ITEM,ITEM     compare ONLY the given pair of languages: i.e
                               'Breton','Breton' or 2,3  (with the latter
                               notation '2' being the 2nd language in the input
                               file)
       --showmanual            show the manual and quit
       --filterscore=NUM       only print results with this score or higher
       --filterbacktrack=NUM   only provide backtracking results for results
                               with this score or higher

```--simplescorefile``` expects a score file for simple unigram based scoring.
An example file is provided under ```scores/defaultSimpleScoring```. For bigram
score files, use a file like ```scores/defaultBigramScoring```.

```--lpblock``` expects a pair of language names (Breton,Breton) or a pair of
integers (3,3 or 4,6) and will then align only the given language pairs with
each other. This option should be very helpful in case you want to parallelize
the program.

```--filterscore``` is used to limit printing results to only the alignments
with score not lower than this option. Given that printing requires a
significant amount of CPU time due to unicode conversion, this option improves
performance substantially.

```--filterbacktrack``` is used to limit printing of backtracking for a given
alignment to the best results. Works like ```--filterscore``` but will always
print the forward result.

```--filternormalized``` applies filter on the length-normalized scores instead
of the absolute ones.



#### Options for bigram alignment variants

    -b --bigramscorefile=ITEM  the file to read the bigram scores from

```--bigramscorefile``` is used to point toward a file with a list of bigram
scores for all language pairs.



## Simple score file description



## Bigram score file description

In contrast to the simple model above, however, character matching is now
performed in a bigram context.

The required score file is currently using an in-house format with the
following columns all required (with whitespace, not tab between the entries):

* language name
* character (bigram 1.1)
* character (bigram 1.2)
* language name
* character (bigram 2.1)
* character (bigram 2.2)
* score

Three example lines

    Albanian_Tosk \' a Albanian_Tosk \' a 4.25238
    Albanian_Tosk \' a Albanian_Tosk \' b 0.402228
    Albanian_Tosk \' a Albanian_Tosk \' g 1.07432



# Example output

The output are four lines for each alignment. An info line with the word ids
(IDS), the alignment score (SCORE), the normalized scores (NSCORE) and the
actual words, started by (WORD) and interleaved by (WORD). The next two lines
are the alignment, with deletions showning up as minus symbols ```-``` in the
deletion field. Note that a deletion does not delete a character from the
input, it merely aligns an existing character in one alignment with the symbol
for deletion ```-``` in the other. The final line provides the per-column score
for the alignment. After the alignment follows one empty line.

Words are written left-to-right in the information line, and bottom to top in
the alignment.

    IDS: 2 3 SCORE:  93.40 NSCORE:  10.38    WORD: ^ h a u s b a u $   WORD: ^ b a u m h a u s $
           b       a       u       m       h       a       u       s       $       -       -       -
           -       -       -       -       h       a       u       s       b       a       u       $
         0.0     0.0     0.0    -1.0    -3.0    33.9    33.8    33.7    -3.0    -1.0     0.0     0.0




# Performance Notes

Measured on a core i5-3570K @ 3.40 GHz; single-threaded.

The program is not compiled for multi-threading, if you need this consider the
```--lpblock``` option first and parallelize on the language pair level.
Otherwise, send a mail.

The running time for calculating 100 000 alignments is:

| Mode      | Simple/Bigram   | Tapes | Alignments per Second |
| :---      | :---            | :---: |                  ---: |
| Global    | Simple          | 2     | 25 000
|           | Bigram          | 2     | 26 600
| Overhang  | Simple          | 2     | 14 300
|           | Bigram          | 2     | 15 160

and when printing alignments via ```--filterscore``` is restricted to scores
```>=10``` to return about 0.6% of alignments:

| Mode      | Simple/Bigram   | Tapes | Alignments per Second |
| :---      | :---            | :---: |                  ---: |
| Global    | Simple          | 2     | 278 000
|           | Bigram          | 2     | 192 500
| Overhang  | Simple          | 2     |  48 125
|           | Bigram          | 2     |  54 400



# Three- and Four-way Alignments

These are currently disabled. If you need them, consider contacting me.



#### Contact

Christian Hoener zu Siederdissen  
Leipzig University, Leipzig, Germany  
choener@bioinf.uni-leipzig.de  
http://www.bioinf.uni-leipzig.de/~choener/  

