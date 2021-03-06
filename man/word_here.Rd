% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/word_here.R
\name{word_here}
\alias{word_here}
\title{Detect words in a list of documents}
\usage{
word_here(text_list, word_list, table = FALSE, group = TRUE, count = FALSE)
}
\arguments{
\item{text_list}{a vector containing all texts to analyse}

\item{word_list}{a list of words imported with the “import_words” function or made with the “make_words” function}

\item{table}{if TRUE, allows to return the result as a table instead of a list}

\item{group}{if TRUE, allows to group the “grouped words” into one result instead of giving a result to each one}

\item{count}{if TRUE, allows to count the searched words in each document instead of only indicate if it’s present or not}
}
\value{

}
\description{
For a list of words imported with the “import_words” function or made with the ‘make_words” function, the “word_here” function searches these words in all given texts.
}
