\name{get_metadata}
\alias{get_metadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Metadata lookup for cMD
}
\description{
Use the OmicIDX API to lookup metadata using an SRP
}
\usage{
get_metadata(srp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{srp}{
An SRP as a string beginning with the prefix "SRP"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame containing sampleID, SRR, sequencing_platform, number_reads, and avg_read_length
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Chloe Mirzayi (chloe.mirzayi@sph.cuny.edu)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
get_metadata("SRP082656")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
