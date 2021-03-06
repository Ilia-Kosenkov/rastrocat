% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05-readme_generator.R
\docType{class}
\name{ReadMeGen}
\alias{ReadMeGen}
\title{ReadMeGen}
\description{
Generator of specific ReadMe files
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-generate_readme}{\code{ReadMeGen$generate_readme()}}
\item \href{#method-generate_data}{\code{ReadMeGen$generate_data()}}
\item \href{#method-set_references}{\code{ReadMeGen$set_references()}}
\item \href{#method-set_bibcode_references}{\code{ReadMeGen$set_bibcode_references()}}
\item \href{#method-set_keywords}{\code{ReadMeGen$set_keywords()}}
\item \href{#method-set_adc_keywords}{\code{ReadMeGen$set_adc_keywords()}}
\item \href{#method-assign_data}{\code{ReadMeGen$assign_data()}}
\item \href{#method-assign_multiple_datasets}{\code{ReadMeGen$assign_multiple_datasets()}}
\item \href{#method-set_remarks}{\code{ReadMeGen$set_remarks()}}
\item \href{#method-new}{\code{ReadMeGen$new()}}
\item \href{#method-clone}{\code{ReadMeGen$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-generate_readme"></a>}}
\if{latex}{\out{\hypertarget{method-generate_readme}{}}}
\subsection{Method \code{generate_readme()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReadMeGen$generate_readme()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-generate_data"></a>}}
\if{latex}{\out{\hypertarget{method-generate_data}{}}}
\subsection{Method \code{generate_data()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReadMeGen$generate_data()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_references"></a>}}
\if{latex}{\out{\hypertarget{method-set_references}{}}}
\subsection{Method \code{set_references()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReadMeGen$set_references(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_bibcode_references"></a>}}
\if{latex}{\out{\hypertarget{method-set_bibcode_references}{}}}
\subsection{Method \code{set_bibcode_references()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReadMeGen$set_bibcode_references(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_keywords"></a>}}
\if{latex}{\out{\hypertarget{method-set_keywords}{}}}
\subsection{Method \code{set_keywords()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReadMeGen$set_keywords(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_adc_keywords"></a>}}
\if{latex}{\out{\hypertarget{method-set_adc_keywords}{}}}
\subsection{Method \code{set_adc_keywords()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReadMeGen$set_adc_keywords(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-assign_data"></a>}}
\if{latex}{\out{\hypertarget{method-assign_data}{}}}
\subsection{Method \code{assign_data()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReadMeGen$assign_data(format, data = NULL, file_name = NULL, desc = NULL)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-assign_multiple_datasets"></a>}}
\if{latex}{\out{\hypertarget{method-assign_multiple_datasets}{}}}
\subsection{Method \code{assign_multiple_datasets()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReadMeGen$assign_multiple_datasets(format, data)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_remarks"></a>}}
\if{latex}{\out{\hypertarget{method-set_remarks}{}}}
\subsection{Method \code{set_remarks()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReadMeGen$set_remarks(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Constructor of \code{ReadMeGen}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReadMeGen$new(cat_id, title, authors, year)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{cat_id}}{Mandatory id of the catalogue / project.}

\item{\code{title}}{Mandatory short title.}

\item{\code{authors}}{A list of authors.}

\item{\code{year}}{Year of puclication.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Constructed object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReadMeGen$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
