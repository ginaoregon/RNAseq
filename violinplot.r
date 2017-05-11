alldata= read.table("~/STAR_filter.txt", header=T, sep="\t",na.strings="NA", fill=T,strip.white=T,blank.lines.skip=F,as.is=T, row.names=1)
countsTable.ori = alldata
countsTable = alldata
conds = c('r1','g1','r3','g3','r4','g4','r5','g5','r7','g7')
conds = as.factor(conds)

alldata.value = countsTable.ori
alldata.log2.value = log2(alldata.value + 1)
alldata.log2 = cbind(row.names=alldata.log2.value, alldata.log2.value)
colnames(alldata.log2) = c("gene", colnames(alldata.value) )
alldata.log2.melt = melt(alldata.log2, id.vars="gene")
pdf("~/all_samples_density_plots_filter.pdf")
p = ggplot(alldata.log2.melt, aes(x=variable, y=value), xlab("Sample") + ylab("log2(read counts)") )
tt = labs(title = expression(paste(log[2], ' raw counts'))) 
p + geom_violin(fill='blue', color='blue', adjust=0.8) + geom_boxplot(width=.1, fill='black', outlier.colour=NA) + stat_summary(fun.y=median, geom='point', fill='white', shape=21, size=2.5) + tt + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())
ggplot(alldata.log2.melt, aes(x=value, color=variable)) + geom_line(stat='density') + expand_limits(y=0) + tt + expand_limits(y=0) + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())
dev.off()

cut1 = 2^3
cut1
countsTable <- subset(countsTable,max > 8)
head(countsTable)
dim(countsTable)

### plot data shapes of filtered data 
### plot data shapes of filtered data 
"Plot data shapes of filtered data"
alldata.value = countsTable
alldata.log2.value = log2(alldata.value + 1)
alldata.log2 = cbind(rownames(alldata.log2.value), alldata.log2.value)
colnames(alldata.log2) = c("gene", colnames(alldata.value) )
alldata.log2.melt = melt(alldata.log2, id.vars="gene")
pdf("~/all_samples_density_plots_filter8.pdf")
p = ggplot(alldata.log2.melt, aes(x=variable, y=value), xlab("Sample") + ylab("log2(read counts)") )
tt = labs(title = expression(paste(log[2], ' raw counts after filtering 8')))
p + geom_violin(fill='blue', color='blue', adjust=0.8) + geom_boxplot(width=.1, fill='black', outlier.colour=NA) + stat_summary(fun.y=median, geom='point', fill='white', shape=21, size=2.5) + tt + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())
ggplot(alldata.log2.melt, aes(x=value, color=variable)) + geom_line(stat='density') + expand_limits(y=0) + tt + expand_limits(y=0) + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())
dev.off()

save.image()