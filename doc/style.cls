\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{style}

\LoadClass[10pt,a4paper]{article}

\RequirePackage{xspace}
\RequirePackage[UTF8]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage[english]{babel}
\RequirePackage[sc,osf]{mathpazo}
\RequirePackage[scaled=.90]{helvet}
\RequirePackage[scaled=.82]{beramono}
\RequirePackage{longtable,array,booktabs}
\RequirePackage{graphicx,textcomp,calc,verbatim,amsmath,xcolor,fullpage}
\RequirePackage{amssymb,listings,enumitem,microtype,hyperref,multicol,caption}

\columnsep 25pt

\linespread{1.05}
\setlength\parskip{1ex}
\setlength\parindent{0pt}
\renewcommand\arraystretch{1.3}
\raggedbottom

\definecolor{comment}{rgb}{0,.5,0}
\definecolor{class}{rgb}{0.1686,.5686,6863}
\definecolor{string}{rgb}{0..6392,0.0824,0.0824}
\definecolor{light}{gray}{0.9286}

\setlist{itemsep=.25pt,leftmargin=20pt,topsep=.5ex}
\setlist[2]{leftmargin=*}
\setdescription{leftmargin=10pt,style=nextline}

\hypersetup{
    linktocpage=false,
    colorlinks=true,
    linkcolor=black,
    citecolor=black,
    filecolor=black,
    urlcolor=black,
    frenchlinks=false,
    breaklinks=true,
    pdffitwindow=true,
    pdfauthor={Martin Ring}
}

\lstdefinelanguage{scala}{%
	keywords={%
		case, default, do, else, for, if, match, while, return, throw, try,%
		catch, finally, abstract, class, def, extends, final, forSome,%
		implicit, import, lazy, new, object, override, package,%
		private, protected, requires, sealed, super, this, trait, type,%
		val, var, with, yield, false, null, true},%
	sensitive=true,%
	comment=[l]{//},%
	morecomment=[s]{/*}{*/},%
	string=[b]"	
}

\lstset{%  
    firstnumber=auto,
	basicstyle=\small\ttfamily,%
	numberstyle=\small\ttfamily\color{class},
	commentstyle=\color{comment},
	keywordstyle=\color{blue},	
	framesep=2.5mm,%
	numbers=left,%
	aboveskip=15pt,%
	belowskip=15pt,%
	captionpos=b,%
	numbersep=5mm,%	
	abovecaptionskip=2.5pt,%
	numberbychapter=true,%
	breaklines=true,%
	xleftmargin=10mm,
	showstringspaces=false,%
	lineskip=1.5pt,%
	tabsize=2,%
	language=scala,%
	stringstyle=\color{string},
	identifierstyle=\idstyle,
	frame=leftline
}

\makeatletter
\newcommand*\idstyle{%
        \expandafter\id@style\the\lst@token\relax
}
\def\id@style#1#2\relax{%
        \ifcat#1\relax\else
                \ifnum`#1=\uccode`#1%
                        \color{class}
                \fi
        \fi
}
\makeatother

\captionsetup[lstlisting]{font=footnotesize,labelfont=sc}
\captionsetup[table]{font=footnotesize,skip=5.5pt,labelfont=sc}

\renewcommand{\quote}[2]{%
\begin{quotation}
	\textit{\frqq{#2}\flqq}%
	\begin{flushright}%
		-- \textsc{{#1}}%
	\end{flushright}%
\end{quotation}%
}

\newcommand{\seeSec}[1]{$\vartriangleright$ \textsc{Abschnitt} \ref{#1}}
\newcommand{\seeTab}[1]{$\vartriangleright$ \textsc{Tabelle} \ref{#1}}
\newcommand{\seeLst}[1]{$\vartriangleright$ \textsc{Listing} \ref{#1}}
\newcommand{\seeUrl}[1]{$\vartriangleright$ \textsc{Url:} \url{#1}}
\newcommand{\key}[1]{\texttt{\textbf{#1}}}

\title{An OOPS Compiler Written in Scala}

\author{Martin Ring}

\date{
	\small%
	Studiengang Informatik\\%
	Universit{\"a}t Bremen\\%
	\today%
	\normalsize %
}
