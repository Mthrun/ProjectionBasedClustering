\name{ShepardDiagram}
\alias{ShepardDiagram}
\title{
Draw a Shepard diagram
}
\description{
This function plots a Shepard diagram of InputDist and OutputDist
}
\usage{
ShepardDiagram(InputDist, OutputDist, xlabel = "Input Distances", 
ylabel = "Output Distances", fancy = F, label = "ProjectionMethod", gPlot = ggplot())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{InputDist}{
Matrix containing the distances of the inputspace.
}
  \item{OutputDist}{
Matrix containing the distances of the outputspace.
}
  \item{xlabel}{
Label of the x axis in the resulting Plot.
}
  \item{ylabel}{
Label of the y axis in the resulting Plot.
}
  \item{fancy}{
Set FALSE for PC and TRUE for publication
}
  \item{label}{
Title of the Shepard diagram
}
  \item{gPlot}{
Ggplot2 object to plot upon.
}
}
\value{
ggplot2 object containing the plot.
}
\author{
Michael Thrun
}