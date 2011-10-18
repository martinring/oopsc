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

%%%%%%%% Seitengestaltung
%\oddsidemargin 0pt
%\evensidemargin 0pt
%\marginparwidth 48pt
%\marginparsep 10pt
%\topmargin -18pt
%\headheight 12pt
%\headsep 25pt
%\footheight 12pt
%\footskip 30pt
%\textheight 625pt
%\textwidth 431pt
\columnsep 25pt
%\columnseprule 1px

\linespread{1.05}
\setlength\parskip{1ex}
\setlength\parindent{0pt}
\renewcommand\arraystretch{1.3}
\raggedbottom

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
    pdfauthor={Fynn Feldpausch, Martin Ring}
}

\lstdefinelanguage{scala}{%
	keywords={%
		case, default, do, else, for, if, match, while, return, throw, try,%
		catch, finally, abstract, class, def, extends, final, forSome,%
		implicit, import, lazy, new, object, override, package,%
		private, protected, requires, sealed, super, this, trait, type,%
		val, var, with, yield, false, null, true},%
	sensitive=false,%
	comment=[l]{//},%
	morecomment=[s]{/*}{*/},%
	string=[b]",%
}

\lstset{%
	numbers=none,%
	basicstyle=\small\ttfamily,%
	framesep=3pt,%
	aboveskip=15pt,%
	belowskip=15pt,%
	captionpos=b,%
	abovecaptionskip=2.5pt,%
	numberbychapter=true,%
	breaklines=true,%
	showstringspaces=false,%
	frame=lines,%
	lineskip=1.5pt,%
	tabsize=2,%
	xleftmargin=.05\linewidth,%
	xrightmargin=.05\linewidth,%
	framexleftmargin=.025\linewidth,%
	framexrightmargin=.025\linewidth,%
	language=scala%
}

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
