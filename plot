#   set up plot to disk as per my special requirements
#   pointsize: the default pointsize of plotted text. The allowed range is
#         '[6, 48]': values outside that range are reset to 12.

# this works for word  (emf is better than wmf)
#   ..later.. I like to reduce the size to 80% in MS word  - it makes the axis
#   lables and other writing a bit smaller (I spuppose I could use cex=.8 or somthing)
myemf <- function(filename, wdt=15, hgt = 12, pdir=plot_dir, has_title = F){
  if(!exists('plot_dir')) plot_dir <- plotDir()
	filename <- paste(pdir, '/', filename, '.emf', sep='')
	win.metafile(file=filename, width=wdt/2.54, height=hgt/2.54)
	cat('Plotting to', filename, '\n')
	my_mai <- c(.925,.795,.05,.2)     # c(bottom, left, top, right)
	if(has_title) my_mai[3] <- my_mai[3] + .15       # add space for main
	par(mai = my_mai)
}

##############################
#From: McGehee, Robert <Robert.McGehee_at_geodecapital.com>
#Date: Sat 14 Jan 2006 - 01:27:08 EST
#  creates plot axis labels in scientific notation

sciNotation <- function(x, digits = 1) {
    if (length(x) > 1) {
        return(append(sciNotation(x[1]), sciNotation(x[-1])))
	}
    if (!x) return(0)
    exponent <- floor(log10(x))
    base <- round(x/10^exponent,digits)
	as.expression(substitute(base %*% 10^exponent,
		list(base = base, exponent = exponent)))
}

##  these functions are called from my_emf

#######################################################################
#       yymmdd date format
yymmdd <- function(thisDate = Sys.Date()){
  return(format(thisDate, '%y%m%d'))
}
#######################################################################
#		
plotDir <- function(pdir = 'plot', ...){
	plot_dir <- paste(pdir, '/', yymmdd(...), sep='')
	dir.create(plot_dir, rec=T)
	return(plot_dir)
}
