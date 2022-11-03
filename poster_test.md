---
title: "Measuring household net energy burdens"
author:
  - name: "Eric Scheier"
    affil: "Emergi"
    main: true
    email: "eric@emergi.eco"
date: "2022-06-15"
poster_height: "24in"
poster_width: "36in"
font_family: "Montserrat"
primary_colour: "#228b2280"
secondary_colour: "#4d4d4d"
accent_colour: "#9e9e9e"
main_width: 0.5
main_fontfamily: "Montserrat"
main_textcol: "#4d4d4d"
main_textsize: "140px"
main_picwidth: "100%"
main_padding_top: "5%"
body_bgcol:	"#9e9e9e20"	#Background colour of the poster's body.
body_textsize: "25px"	#Size of any paragraph text found in the poster.
body_textcol:	"#000000"	#Colour of the body text.
title_textsize: "80pt"	#Text size for the poster title if title is given.
author_textsize: "1.17em"	#Text size for author output (this is only for the option where author has main: true).
author_textcol:	none	#Colour of the author text. Use hex values for easiest use. Will overrule the 'primary' colour if set.
authorextra_textsize:	"35px"	#Text size of all author names if they are note listed as main:true.
affiliation_textsize:	"25px"	#Text size of the affiliation output.
affiliation_textcol:	'#00000060'	#Colour of the affiliation text output.
caption_textsize:	"20pt"	#Text size for any caption text generated by figures or tables in the document.
reference_textsize:	"25px"	#Text size of the automated Reference section if used.
column_padding:	"10mm"	#Padding size for the content of the body section from the edge of the poster as well as from the edge of the main section.
link-citations:	true	#Will make inline citations a clickable link which will direct the reader to the appropriate portion of the "References" section.
bibliography: Poster.bib	#File path to a .bib bibliography file if needed.
csl: nature-no-et-al.csl	#File path to a .csl file which will change the citation style for the document, many options can be found at on the Zotero Styles Repository
output:
  posterdown::posterdown_betterland:
    self_contained: false
#pandoc_args: --mathjax
#highlight: espresso
    number_sections: false
    template: poster_template.html
---
  
  
  
  
  
  
  
  
  
  
  












---
  main_findings:
  - "**_1 in 6_ American homes faces energy poverty**"
- '![](poster_map.svg){.main_pic}'
logoleft_name: ![](emergi_logo.png){.main-img-left}{width=250px}
logoright_name: ![](qr.svg){.main-img-right}{width=250px}
---
  
  # Introduction
  
  Energy burden is a standard indicator of energy affordability and equity.

\[G = Gross\ Income\ ;\ S = Spending\ on\ Energy\]
\[
  Energy\ Burden\ (E_{b})= \frac{S}{G}
  \]

Can we build on the energy burden framework by assessing __*net*__ energy burdens at a macro-energy systems scale? Yes[@scheierMeasurementStrategyAddress2022].

\[
  Household\ Net\ Energy\ Return\ (N_{h}) = \frac{G - S}{S}
  \]

# Methods

Redefine our net energy metric as a percentage to improve understanding:
  
  \[
    Net\ Energy\ Burden\ (E^{n}_{b}) = \frac{S}{G - S}
    \]

Apply the Net Energy Burden ($E_b^n$) framework to estimates of spending and income for the U.S. residential housing stock[@maLowIncomeEnergyAffordability2019] to determine how many homes' __net energy burdens__ lie below a standard energy poverty line of **$E_b^n \approx 6\%$**.

\[E_{B}^{*} = \frac{S}{G} = 6\%\ \Rightarrow \ E^{n*}_{b} \approx 6\%\ ;\ N_{h}^{*} \approx 16\]

# Results

+ **1 in 6** homes in the U.S. faces energy poverty.
+ **1 in 4** homes facing energy poverty is above the US Federal Poverty Line
+ **Solar** powered homes experience energy poverty **50% as often** as other homes
+ Homes in minority communities experience energy poverty **2x as often** as other homes

# Ongoing Work

## Sonoma Clean Power Authority

![](doe_eere_logo.jpg){width=350px} ![](scp_logo.svg){width=350px}




<img src="MES_2022_poster_files/figure-html/show-scp-chart-1.png" width="100%" style="display: block; margin: auto;" />

This work is funded by the Solar Energy Innovators Program of the United States Department of Energy's Office of Energy Efficiency & Renewable Energy and hosted by Sonoma Clean Power Authority, the public power provider for 240,000 residents of California's Sonoma and Mendocino Counties in the United States.

## Emergi

![](emergi_logo_wide.png){width=350px} ![](UNC_logo_RGB.svg){width=350px}




<img src="MES_2022_poster_files/figure-html/show-serc-chart-1.png" width="100%" style="display: block; margin: auto;" />

Emergi is a cooperative energy provider which connects its members with local solar farms throughout the Southeastern United States, supported by the University of North Carolina - Chapel Hill and its member-owners.

# References