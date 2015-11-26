[![Build Status](https://travis-ci.org/choener/WordAlignment.svg?branch=master)](https://travis-ci.org/choener/WordAlignment)

# Word alignments in natural languages

This library and program are designed for the alignment of *words* in human
languages. Each word is encoded as a list of *characters*. A single
*character*, however, is encoded as a *list of unicode symbols*, not just a
single symbol.

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



The WordAlignment program comes with two modes, *twowaysimple* and *twoway*.

## TwoWay - Simple

```WordAlign twowaysimple``` aligns words based on a simple scoring model. Gaps
are scored with linear costs. Matches are scored based on a simple scoring file
handling unigrams of characters. Even the simple model allows for the concept
of equivalence classes. This makes it possible to not only score exact matches,
but to give somewhat high scores to, say, two vowels that should be matched.

The following command-line options are provided:

    --scorefile=ITEM
    --lpblock=ITEM,ITEM
    --showmanual
    --prettystupid
    --outfile=ITEM

```--scorefile``` is the simple score file to be used by the mode. the
*defaultSimpleScoring* file can be used as a template.

```--lpblock``` expects a pair of language names (Breton,Breton) or a pair of
integers (3,3 or 4,6) and will then align only the given language pairs with
each other. This option should be very helpful in case you want to parallelize
the program.

```--showmanual``` will show this manual in plain text.

```--prettystupid``` will show a progress bar of the current language pair.
It's pretty and helpful with smaller tasks but should not be used when you
parallelize on a grid engine.

```--outfile``` writes to the given output file, not stdout. Actually required
when ```--prettystupid``` is active.



## TwoWay (Complex Scoring)

The complex scoring model uses linear gap costs. In contrast to the simple
model above, however, character matching is now performed in a bigram context.

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

In addition to the scoring file, set via ```--scorefile```, the default score
constant and the gap cost constant need to be set.

```--bigramdef=NUM``` is the score given to unknown matches.

```--gapopen=NUM``` is the score given to each gap character (and not just the
opening score)

```--lpblock``` as above

```--showmanual``` as above

```--prettystupid``` as above

```--outfile``` as above

An example output is given below.

The output are four lines for each alignment. An info line with the word ids
(IDS), the alignment score (SCORE), the normalized scores (NSCORE) and the
actual words, started by (WORDS) and interleaved by (WORD). The next two lines
are the alignment, with deletions showning up as minus symbols (---) in the
deletion field. Note that a deletion does not delete a character from the
input, it merely aligns an existing character in one alignment with the symbol
for deletion (--) in the other. The final line provides the per-column score
for the alignment. After the alignment follows one empty line.

The normalized score is defined as SCORE / maximum of input word lengths

```cat albanian.input | head -n 3 | ./dist/build/WordAlign/WordAlign twoway -s albanian.scores```

yields:

    IDS: 1 2 SCORE: 32.85 NSCORE: 5.48    WORDS: ^ d o u a r $   WORD   ^ d o u a r $
       ^   d   o   u   a   r   $
       ^   d   o   u   a   r   $
     0.0 4.8 5.9 4.1 6.6 4.6 6.8
    
    IDS: 1 3 SCORE: -12.36 NSCORE: -1.77    WORDS: ^ d o u a r $   WORD   ^ p o u l t r $
         ^     d     -     o     u     a     r     -     -     $
         ^     p     o     -     u     -     l     t     r     $
       0.0   0.3  -5.0  -5.0   4.1  -5.0 -20.0  -5.0  -5.0   6.8



## Performance Notes



#### Contact

Christian Hoener zu Siederdissen  
Leipzig University, Leipzig, Germany  
choener@bioinf.uni-leipzig.de  
http://www.bioinf.uni-leipzig.de/~choener/  

