% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/units_tracking.R
\name{units_tracking}
\alias{units_tracking}
\title{Group extracted values into an organized table}
\usage{
units_tracking(data, word_list)
}
\arguments{
\item{data}{table of data imported with the “import_data” function}

\item{word_list}{a list of words imported with the “import_words” function or made with the “make_words” function}
}
\value{

}
\description{
Allows to extract values of interest into the texts and group them into a table with columns indicating the text ID, the patient ID and the date. The research is done with words having an indicated “unit type”. This is done from the data table imported with the “import_data” function and from the words list imported with the “import_words” function or made with the “make_words” function.
}
