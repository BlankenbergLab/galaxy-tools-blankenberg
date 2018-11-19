#!/usr/bin/env Rscript 
library(optparse)
library(gPCA)


# parse options
option_list = list(
   make_option(
    c("--version"),
    action = "store_true",
    default = FALSE,
    help = "Print version and exit"
   ),
  make_option(
    c("-i", "--batch"),
    action = "store",
    default = NA,
    type = 'character',
    help = "Input batch"
  ),
  make_option(
    c("-d", "--data"),
    action = "store",
    default = NA,
    type = 'character',
    help = "Input data"
  ),
   make_option(
    c("-n", "--nperm"),
    action = "store",
    default = 1000,
    type = 'numeric',
    help = "Number of permutations to preform"
   ),
   make_option(
    c("-c", "--center"),
    action = "store_true",
    default = FALSE,
    help = "Is the input batch and data centered [default %default]"
	 ),
   make_option(
    c("-y", "--scaleY"),
    action = "store_true",
    default = FALSE,
    help = "Scale to Y axis"
   ),
   make_option(
    c("-f", "--filt"),
    action = "store",
    default = NULL,
    type = 'numeric',
    help = "Retain features"
	 ),
   make_option(
    c( "--npcs"),
    action = "store",
    default = 3,
    type = 'numeric',
    help = "Number of principal components to plot"
   ),
   make_option(
    c("-p", "--path"),
    action = "store",
    default = '$html_outfile.extra_files_path', 
    type = 'character',
    help = "File path"
   ),
   make_option(
    c("-s", "--seed"),
    action = "store",
    default = NULL,
    type = 'numeric',
    help = "Set a seed number"
   ),
   make_option(
    c("-x", "--numbers_outfile"),
    action = "store",
    default = NA, 
    type = 'character',
    help = "Numbers output"
 	 ),
   make_option(
    c("-o", "--html_outfile"),
    action = "store",
    default = NA,
    type = 'character',
    help = "Output",
  )
)

opt <-parse_args(OptionParser(option_list = option_list))

if (opt$version){
  # print version and exit
  cat(paste("gPCA version", toString(packageVersion("gPCA"))), "\n")
  quit()
}

# Check parameter values

if ( ! file.exists(opt$batch)){
  stop((paste('File for batch', opt$batch, 'does not exist')))
}
if ( ! file.exists(opt$data)){
  stop((paste('File for data', opt$data, 'does not exist')))
}

batch<-as.numeric(factor(as.matrix(read.delim(opt$batch, header=TRUE, row.names=1, sep="\t"))))

data<-t(as.matrix(read.delim(opt$data, row.names=1, header=TRUE, sep="\t")))

out<-gPCA.batchdetect(x=data, batch=batch, center=opt$center, scaleY=opt$scaleY, nperm=opt$nperm, filt=opt$filt, seed=opt$seed)

out$varPCg1<-((out$varPCg1-out$varPCu1)/out$varPCg1)*100

dir.create(opt$path);  

write.table(data.frame(out$delta, out$p.val, out$varPCg1),file=opt$numbers_outfile, sep="\t", row.name=FALSE, quote=FALSE)

# General Distribution 
par(mai=c(0.8,0.8,0.1,0.1),cex=0.8)
png(paste(opt$path,'/gDist.png', sep=""), width=1020, height=800, units='px')
gDist(out)
invisible(dev.off())


# Guided/Unguided 1v2
par(mai=c(0.8,0.8,0.1,0.1),cex=0.8)
png(paste(opt$path,'/guided_1v2.png', sep=""), width=1020, height=800, units='px')
PCplot(out,ug="unguided",type="1v2")
invisible(dev.off())

par(mai=c(0.8,0.8,0.1,0.1),cex=0.8)
png(paste(opt$path,'/unguided_1v2.png', sep=""), width=1020, height=800, units='px')
PCplot(out,ug="unguided",type="1v2")
invisible(dev.off())


# Guided/Unguided comp,3
par(mai=c(0.65,0.65,0.1,0.1),cex=0.8)
png(paste(opt$path,'/guided_npcs.png', sep=""), width=1020, height=800, units='px')
PCplot(out,ug="guided",type="comp",npcs=opt$npcs)
invisible(dev.off())

par(mai=c(0.65,0.65,0.1,0.1),cex=0.8)
png(paste(opt$path,'/unguided_npcs.png', sep=""), width=1020, height=800, units='px')
PCplot(out,ug="unguided",type="comp",npcs=opt$npcs)
invisible(dev.off())


# Guided/Unguided CumlativeVarPlot
par(mai=c(0.8,0.8,0.1,0.1),cex=0.8)
png(paste(opt$path,'/guided_var.png', sep=""), width=1020, height=800, units='px')
CumulativeVarPlot(out,ug="guided",col="blue")
invisible(dev.off())

par(mai=c(0.8,0.8,0.1,0.1),cex=0.8)
png(paste(opt$path,'/unguided_var.png', sep=""), width=1020, height=800, units='px')
CumulativeVarPlot(out,ug="unguided",col="blue")
invisible(dev.off())


write(paste('<html>
<table id="r-gpca-wrap" align="center" border="1">
 <tr>
  <th>Delta</th><th>P-value</th><th>varPCg1</th>
 </tr>
 <tr>
  <td id=delta>',out$delta,'</td><td id=p.val>',out$p.val,'</td><td id=varPCg1>',out$varPCg1,'</td>
 </tr>
</table>

<center><img src="gDist.png"/></center><br>
<center><title>Guided 1v2</title><br><img src="guided_1v2.png"/></center><br>
<center><title>Unguided 1v2</title><br><img src="unguided_1v2.png"/></center><br>

<center><title>Guided Compare to ',opt$npcs,'</title><br><img src="guided_npcs.png"/></center><br>
<center><title>Unguided Compare to ',opt$npcs,'</title><br><img src="unguided_npcs.png"/></center><br>

<center><title>Guided Cumulative Variance</title><br><img src="guided_var.png"/></center><br>
<center><title>Unguided Cumulative Variance</title><br><img src="unguided_var.png"/></center><br>

</html>'
),file = opt$html_outfile)

sessionInfo()
