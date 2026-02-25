\name{NEWS}
\title{News for Package \pkg{FlexParamCurve}}
\description{News for Package \pkg{FlexParamCurve}}

\section{Changes in version 1.5.7}{
  \itemize{
    \item updated Rd cross-references as required for R packages
  }
}

\section{Changes in version 1.5.6}{
  \itemize{
    \item Disagreement between package versions in the help files is resolved
    \item Models that fail to fit are now assessed in the code using \code{inherits} rather than \code{class} "==" to resolve several issues with failed fitting of very complex models and fix errors with \code{pn.modselect.step}
  }
}

\section{Changes in version 1.5.5}{
  \itemize{
    \item Updated to work on new R code requirements
    \item Fixed 4 bugs from 1.5.3 (FlexParamCurve 1.5.4 was not generally circulated):
    \item 1) \code{verbose} from modpar is now transitioning to \code{pn.mod.compare} and \code{pn.modselect.step}
		\item  2) \code{modpar}, \code{pn.mod.compare} and \code{pn.modselect.step} will now throw an error if any data values (\code{x}, \code{y} or \code{grp}) are \code{NA} 
		\item 3) in \code{pn.mod.compare} and \code{pn.modselect.step}, \code{existing = TRUE} now works and will not fit any existing models within the working environment. These are transfered to the package environment, used, and transfered back
		\item 4) optional constraints on either end of curve (\code{first.y}, \code{x.at.first.y}, \code{last.y}, \code{x.at.last.y}) are now functioning

  }
}


\section{Changes in version 1.5.3}{

  \itemize{
    \item (note FlexParamCurve 1.5.2 was not generally circulated thus changes detailed are additions since 1.5.1)
    \item  Change in default approach to fitting monotonic curves:
      Monotonic curves are exemplified by the logistic and Richards curve, and do not incorporate a recessional part of the curve. In previous versions, \pkg{FlexParamCurve} fixed recessional parameters to the values in the specified pn.options object (see
      \code{?modpar}). However, if data used in modpar supported recessional parameters non-intuitive recessional plots were generated using monotonic curve models (\code{modno} 12 
      or 32). The new approach implemented in v1.5.3 is to by default ignore recessional parameters whenever modno 12 or 32 are optimized or plotted. To access the original approach (for those who wish to optimize pre-recession parameters only but retain fixed recession trajectories) set \code{force.nonmonotonic = TRUE} in \code{modpar}
      \item Update of model selection routines (\code{pn.mod.compare} and \code{pn.modselect.step}) 
      The new default for \code{pn.mod.compare} is to proceed using variable models (those with a shape parameter, M, for the pre-recessional part of the curve) for subsequent model selection if neither of the initial models successfully converge. User preferences can be selected using either \code{forcemod = 3} or \code{forcemod = 4}; see \code{?pn.mod.compare}. In both selection routines the code has been adjusted to make \code{forcemod} work appropriately.
      \item Implementation of required package dependencies: The new dependency information required by CRAN has been incorporated.
      }
}

\section{Changes in version 1.5.1}{

\itemize{
  \item IMPORTANT: Change in the use of the Global Environment: In previous versions, FlexParamCurve has automatically written the \code{pn.options} file (with fitting estimates and user custom fitting instructions) to the Global Environment. Additionally, the model selection routines outputed all the fitted \code{nlsList} models to the Global Environment. To comform to R good practice for packages, the user can now specify that the Global Environment is not written to. To make this version back-compatible with previous \code{FlexParamCurve} versions, the default is to write to the Global Environment but this can be switched off setting the parameter \code{Envir} to a value for an alternative environment to write to. Internal parameters are written to the namespace environment, \code{FlexParamCurve:::FPCEnv}. This applies to the following function calls: \code{modpar}, \code{SSposnegRichards}, \code{pn.mod.compare}, \code{pn.modselect.step}, \code{posnegRichards.eqn} and \code{change.pnparameters}.
      
    \item Addition of new function: \code{get.mod}.This is mostly used internally but can be used by user to copy files from one environment to another. By default this will copy files output from model selection routines (\code{pn.mod.compare} and \code{pn.modselect.step}) from the packge namespace to the global environment.
      
    \item Correction of faulty examples. FlexParamCurve v1.4.3 was archived for reason of failing examples. These have been revised accordingly and pass appropriate tests.
     \item Addition of a NEWS file: This file contains news for all recent versions of FlexParamCurve that were previously found at https://pennstate.academia.edu/SteveOswald/Posts Please check this website for instant updates
     \item Use of Lazy Loading for data files:The package now uses lazyloading for all data files used in examples. Thus all data      files can be used directly without the need to call \code{data} beforehand.
     \item Bug fix for \code{SSposnegRichards}: Correction of a problem for selection routines \code{pn.mod.compare} and \code{pn.modselect.step} that was caused by failing to specify \code{taper.ends} values explicitly when initially running \code{modpar}. These two selection routines will now work alone whether specifying \code{taper.ends} or not and regardless of previous runs of \code{modpar}.
    \item Error message update in \code{pn.modselect.step}: Addition of informative error message for this selection routine if no \code{nlsList} models are successfully fitted - induces safe termination of the routine with appropriate error message.
      }
}

\section{Changes in version 1.4.3}{

\itemize{
\item FlexParamCurve Documentation available: Paper, Online Tutorial and Introduction to the R package: Methods in Ecology and Evolution recently published a showcase paper on FlexParamCurve and is hosting two videos on the package, as online extras. These include an ~8-min introduction as to the purpose and utility of the package and a 12-min tutorial in R running you through some of the main functions in FlexParamCurve, including curve selection, model plotting and using FlexParamCurve models within mixed effects models. 
   The paper is available at: http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2012.00231.x/abstract. 
   Videos can be found at: http://www.methodsinecologyandevolution.org/view/0/podcasts.html
  \item Major code updates have been made to the package in this version:	 
  \code{selfStart} ( \code{SSposnegRichards}) and model selection routines run ~ 10 x faster and are more reliable!
	more user-options to modpar and model selection routines to allow tapering at either end of the curve ( \code{taper.ends})
  custom specification of parameter bounds ( \code{width.bounds})
  passing arguments to  \code{nls} and  \code{nlsList} within  \code{modpar}
  automated bounds expansion for data exhibiting very different curves between different levels
      }
}

\section{Changes in version 1.4.0}{

\itemize{
\item \code{modpar} and \code{SSposnegRichards} require users to specify the name of a list object to write model parameters and fitting options to, so that multiple models can be fitted alternatively within one session
\item Various inconsistent names of functions have been updated
\item This new version is showcased in a forthcoming Methods in Ecology and Evolution Application
     paper
\item Thanks to Timothy Paine for his helpful suggestions that refined this version of FlexParamCurve
        }
}

\section{Changes in version 1.2.0}{

\itemize{
\item Significant improvement in the accuracy of optimizations so this version is to be prefered over v1.1. As in version 1.1, it can be used for a variety of complex parameteric growth curves or any relationships that exhibit multiple S-shaped curves between predictors and response variables
\item Added flexibility to model curves that follow a very general double-Richards type (not just positive-negative trajectory), e.g. it can now perform double-positive (or double-negative formulations), e.g. Meyer, P.(1994) Bi-logistic growth. Technological Forecasting and Social Change. 47: 89-102)
\item Ability to model two component models (where separate curves do not interact) as well as the traditional double curves. e.g. of two component models: Ross, J.L. et al. (1994) Age,growth, mortality, and reproductive biology of red drums in North Carolina waters. Transactions of the American Fisheries Society 124: 37-54. OR Murphy, S. et al. (2009) Importance of biological parameters in assessing the status of Delphinus delphis. Marine Ecology Progress Series 388: 273-291
\item It can estimate double-positive richards and double negative richards and should be able to accomodate a wide variety of data [if you have a dataset that doesn't fit then let me know and I'll work it in]
\item It is slightly slower for more complex datasets: speeding up optimization algorithms is in the pipeline
\item Option \code{verbose} added for use in \code{modpar} and \code{SSposnegRichards} to allow trouble shooting
      }
}

\section{Changes in version 1.1.0}{

\itemize{
\item I am happy to announce that the FlexParamCurve R package is now available
     on CRAN (http://cran.r-project.org/web/packages/FlexParamCurve/index.html)
\item This package provides a flexible selfStart function and model selection tools for fitting \code{nls}, \code{nlsList} and \code{nlme} models for data that follow positive and negative trajectories. The package was designed with growth analysis (see Huin & Prince 2000. Chick Growth in Albatrosses: Curve Fitting with a Twist. J. Avian Biol. 31(3): 418-425) in mind but provides support for any curve-fitting requiring combined positive-negative trajectories
\item It can be downloaded directly from R in the usual way: install.packages("FlexParamCurve") 
        }
}


