% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_info.R
\name{get_info}
\alias{get_info}
\title{Get info}
\usage{
get_info(
  df,
  prompt,
  conditions = NA,
  n_movs = 20,
  token = Sys.getenv("OPENAI_API_KEY")
)
}
\arguments{
\item{df}{movs tibble}

\item{prompt}{path to GPT prompt}

\item{conditions}{conditions}

\item{n_movs}{lines from \code{df} used for prompt}

\item{token}{= OpenAI token}
}
\description{
get process info from movs using GPT
}
