
#' helper function for app
#'
#' @param manifest list of facets run directories
#' @param progress progress bar from shiny
#' @return simple metadata data.frame
#' @import dplyr
#' @export load_samples
load_samples <- function(manifest, progress) {
  metadata <- data.frame(sample=c(), path = c(), run_dir_exists = c(), num_fits_found = c(),
                         manually_reviewed = c(), best_fit_available = c(), stringsAsFactors=FALSE)
  for(i in 1:length(manifest)[1]) {
    sample_path = manifest[i]
    sample = tail(unlist(strsplit(sample_path, "/")), 1)
    m <- metadata_init_quick(sample, sample_path)
    metadata <- rbind(metadata,
                      data.frame(sample=sample, path=sample_path, run_dir_exists = m[1], num_fits_found = m[2],
                                 manually_reviewed = m[3], best_fit_available=m[4], review_date = m[5], stringsAsFactors=FALSE ))
    progress$inc(1/length(manifest), detail = paste(" ", i, "/", length(manifest)))
  }
  metadata
}

#' helper function for app
#'
#' @param sample sample name
#' @param sample_path path to facets run dir
#' @return minimal description of the facets run
#' @export metadata_init_quick
metadata_init_quick <- function(sample, sample_path) {
  run_dir_exists = "No"
  if ( dir.exists(sample_path)) {
    run_dir_exists = "Yes"
  }
  facets_run_dirs = list.dirs(sample_path, full.names=FALSE)
  facets_run_dirs <- facets_run_dirs[grep("^facets", facets_run_dirs)]

  review_file = paste0(sample_path, "/facets_review.manifest")
  manually_reviewed = "No"
  best_fit_available = "No"
  review_date = "NA"

  if ( file.exists(review_file) ) {
    df <- fread(review_file, skip = 1) %>%
      arrange(desc(date_reviewed))
    if ( dim(df)[1] > 0 ){
      manually_reviewed = "Yes"
      best_fit_available = ifelse ( df$review_status[1] == "reviewed_best_fit", "Yes", "No")
      review_date = df$date_reviewed[1]
    }
  }
  return (c(run_dir_exists, length(facets_run_dirs), manually_reviewed, best_fit_available, review_date))
}

#' helper function for app
#'
#' @param sample sample name
#' @param sample_path path to facets run dir
#' @param progress progress bar from shiny
#' @return description of the facets run
#' @export metadata_init
metadata_init <- function(sample, sample_path, progress) {
  facets_runs <- get_new_facets_runs_df()
  facets_run_dirs = list.dirs(sample_path, full.names=FALSE)
  facets_run_dirs <- facets_run_dirs[grep("^facets", facets_run_dirs)]

  for(fi in 1:length(facets_run_dirs)) {
    progress$inc(1/length(facets_run_dirs), detail = paste(" ", fi, "/", length(facets_run_dirs)))
    fit_name = facets_run_dirs[fi]
    facets_run = paste(sample_path, "/", fit_name, sep="")
    facets_run_files = list.files(facets_run, pattern=".out$")
    if ( length(facets_run_files) == 0 ) {
      next
    }

    for( fif in 1:length(facets_run_files) ) {
      paste(facets_run, facets_run_files[fif], sep="/")
      facets_out_params = readLines(paste0(facets_run, "/", facets_run_files[fif]))
      run_type = "hisens_"
      if (grepl("_purity", facets_out_params[2])) {
        run_type = "purity_"
      }

      rm(list = ls()[grep(run_type, ls())]) # remove all previous facets_params
      run_prefix = ""
      for ( p_idx in 1:length(facets_out_params)) {
        line = gsub(" |#", "", facets_out_params[p_idx])
        sp = unlist(strsplit(line, "="))
        if ( length(sp) == 2) {
          if (sp[1] == "TAG"){
            run_prefix = paste0(facets_run, "/", sp[2])
            assign(paste0(run_type, "prefix"), run_prefix )
          }
          assign(paste0(run_type, sp[1]), sp[2])
        }
      }
      rdata_file = paste0(run_prefix, ".Rdata")
      if ( file.exists(rdata_file)) {
        load(rdata_file)
        if (!is.null(out$alBalLogR)) {
          assign(paste0(run_type, "alBalLogR"), paste(round(out$alBalLogR[,1],digits = 2), collapse=", "))
        }
      }
    }

    facets_runs <- rbind(facets_runs,
                         data.frame(tumor_sample_id = sample, path = sample_path, fit_name = fit_name,
                                    purity_run_prefix = get0("purity_prefix", ifnotfound = NA),
                                    purity_run_Seed = get0("purity_Seed", ifnotfound = NA),
                                    purity_run_cval = get0("purity_cval", ifnotfound = NA),
                                    purity_run_Purity = round_down(get0("purity_Purity", ifnotfound = NA)),
                                    purity_run_Ploidy = round_down(get0("purity_Ploidy", ifnotfound = NA)),
                                    purity_run_dipLogR = round_down(get0("purity_dipLogR", ifnotfound = NA)),
                                    purity_run_alBalLogR = get0("purity_alBalLogR", ifnotfound = NA),

                                    hisens_run_prefix = get0("hisens_prefix", ifnotfound = NA),
                                    hisens_run_Seed = get0("hisens_Seed", ifnotfound = NA),
                                    hisens_run_cval = get0("hisens_cval", ifnotfound = NA),
                                    hisens_run_hisens = round_down(get0("hisens_hisens", ifnotfound = NA)),
                                    hisens_run_Ploidy = round_down(get0("hisens_Ploidy", ifnotfound = NA)),
                                    hisens_run_dipLogR = round_down(get0("hisens_dipLogR", ifnotfound = NA)),

                                    manual_note = NA,
                                    is_best_fit = NA,
                                    stringsAsFactors=FALSE
                         )
    )
  }
  facets_runs
}


#' helper function for app
#'
#' @param sample sampleid
#' @param sample_path facets run directory containing 'facets_review.manifest'
#' @return converts string to numeric and rounds to 2-digits
#' @export get_review_status
get_review_status <- function(sample, sample_path) {
  review_file = paste0(sample_path, "/facets_review.manifest")
  if ( !file.exists( review_file )) {
    df <- data.frame(
      sample = character(),
      path = character(),
      review_status = character(),
      best_fit = character(),
      review_notes = character(),
      reviewed_by = character(),
      date_reviewed = as.POSIXct(character()),
      stringsAsFactors=FALSE
    )
    return(df)
  }
  return (fread(review_file, colClasses=list(POSIXct="date_reviewed"), skip = 1))
}

#' helper function for app
#'
#' @param sample_path  facets run directory containing 'facets_review.manifest'
#' @param df dataframe
#' @return converts string to numeric and rounds to 2-digits
#' @export update_review_status_file
update_review_status_file <- function(sample_path, df) {
  if (is.null(df) || dim(df)[1] == 0) {
    return (FALSE)
  }
  review_file = paste0(sample_path, "/facets_review.manifest")
  if ( !file.exists(review_file)) {
    con = file(review_file)
    writeLines("# generated by facets-preview app. DO NOT EDIT.", con)
    close(con)
    write.table(df, review_file, append=TRUE, sep="\t", row.names=F, quote=F)
  }
  else {
    write.table(df, review_file, append=TRUE, sep="\t", row.names=F, quote=F, col.names = F)
  }
  return (TRUE)
}

#' helper function for app
#'
#' @param val_str input string
#' @return converts string to numeric and rounds to 2-digits
#' @export round_down
round_down <- function(val_str){
  if ( is.na(val_str)) {
    return (NA)
  }

  if ( grepl("^\\-?\\d*\\.?\\d*$", as.character(val_str)) ) {
    return (round(as.numeric(val_str), digits=2))
  } else {
    val_str
  }
}

#' helper function for app
#'
#' @return returns an empty dataframe
#' @export get_new_facets_runs_df
get_new_facets_runs_df <- function() {
  return (data.frame(
    tumor_sample_id = c(), path = c(), fit_name = c(),

    purity_run_prefix = c(), purity_run_Seed = c(), purity_run_cval = c(),
    purity_run_Purity = c(), purity_run_Ploidy = c(), purity_run_dipLogR = c(), purity_run_alBalLogR = c(),

    hisens_run_prefix = c(), hisens_run_Seed = c(), hisens_run_cval = c(),
    hisens_run_hisens = c(), hisens_run_Ploidy = c(), hisens_run_dipLogR = c(),

    manual_note = c(),
    is_best_fit = c(),
    stringsAsFactors=FALSE
  )
  )
}

#' helper function for app
#'
#' @return launches app
#' @export launch_application
launch_application <- function() {
  shiny::runApp(appDir = system.file("application", package = "facetsPreview"))

}

#' helper function for app
#'
#' @return launches app
#' @export launch_application_browser
launch_application_browser <- function() {
  shiny::runApp(appDir = system.file("application", package = "facetsPreview"), launch.browser = TRUE)
}

require(bit64)
require(Cairo)
require(ggplot2)
require(grid)
require(gridExtra)
require(plyr)
require(data.table)

#' helper function for app
#'
#' @param out
#' @param fit
#' @return simple metadata data.frame
#' @import bit64
#' @import Cairo
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import plyr
#' @import data.table
#' @export copy.number.log.ratio
copy.number.log.ratio = function(out, fit, load.genome=FALSE, gene.pos=NULL, col.1="#0080FF", col.2="#4CC4FF", sample.num=NULL, lend='butt', theme='bw', subset.indices=NULL){

  mat = out$jointseg
  mat = subset(mat, chrom < 23)
  mat = get.cumulative.chr.maploc(mat, load.genome)
  mid = mat$mid
  mat = mat$mat

  cncf = fit$cncf
  cncf = subset(cncf, chrom < 23)
  dipLogR = out$dipLogR

  cnlr.median = rep(cncf$cnlr.median, cncf$num.mark)
  mat = cbind(mat, cnlr.median)

  starts = cumsum(c(1,cncf$num.mark))[1:length(cncf$num.mark)]
  ends = cumsum(c(cncf$num.mark))
  my.starts = mat[starts,c('chr.maploc','cnlr.median')]
  my.ends = mat[ends,c('chr.maploc','cnlr.median')]

  if(is.null(sample.num)){subset_ = 1:nrow(mat)}

  if(is.null(sample.num) == FALSE){
    if(sample.num >= nrow(mat)){subset_ = 1:nrow(mat)}
    if(sample.num < nrow(mat)){subset_ = sort(sample(1:nrow(mat), sample.num, replace=FALSE))}
  }

  if (!is.null(subset.indices)) {
    mat = mat[subset.indices,]
  } else { mat = mat[subset_,] }

  col.rep = 1 + rep(mat$chrom - 2 * floor(mat$chrom/2))
  pt.cols = c(col.1, col.2)[col.rep]

  ymin = floor(min(range(cncf$cnlr.median), na.rm = T))
  if (ymin > -3) ymin = -3

  cnlr = ggplot(mat, environment = environment())
  if(!is.null(gene.pos)){
    cnlr = cnlr + geom_vline(xintercept=gene.pos$mid, color='palevioletred1')
    mat$gene = FALSE
    mat$gene[which(mat$chrom == gene.pos$chrom & mat$maploc >= gene.pos$start & mat$maploc <= gene.pos$end)] = TRUE
  } else { mat$gene = FALSE }

  cnlr = cnlr +
    #geom_point(aes(y=cnlr,x=chr.maploc), colour=pt.cols, size=.4) +
    geom_point(aes(y=cnlr,x=chr.maploc), pch = 19, col=pt.cols, size=.4) +
    geom_point(data = subset(mat, gene==T), aes(y=cnlr,x=chr.maploc), color='#525252', size=.4) +
    scale_x_continuous(breaks=mid, labels=names(mid)) +
    xlab('') +
    scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(ymin ,3)) +
    ylab('Log Ratio') +
    geom_hline(yintercept = dipLogR, color = 'sandybrown', size = .8) +
    geom_segment(data=cncf,aes(x=my.starts$chr.maploc, xend=my.ends$chr.maploc, y=my.starts$cnlr.median, yend=my.ends$cnlr.median), col='red3', size=1, lineend=lend)

  panel.grid.col='white'; grid.width = .5
  if(theme=='bw'){panel.grid.col='grey'; grid.width = .2; cnlr = cnlr + theme_bw()}

  cnlr = cnlr + theme(axis.text.x  = element_text(angle=0, size=8),
                      axis.text.y = element_text(angle=0, size=8),
                      text = element_text(size=10),
                      panel.grid.minor.x=element_line(colour=panel.grid.col, size=grid.width),
                      panel.grid.major.x=element_line(colour=panel.grid.col, size=0),
                      plot.margin = unit(c(0,1,0,0), 'lines'))
  cnlr
}

#' helper function for app
#'
#' @param out
#' @return simple metadata data.frame
#' @import dplyr
#' @export var.allele.log.odds.ratio
var.allele.log.odds.ratio = function(out, fit, load.genome=FALSE, gene.pos=NULL, col.1="#0080FF", col.2="#4CC4FF", sample.num=NULL, lend='butt', theme='bw', subset.indices=NULL){

  mat = out$jointseg
  mat = subset(mat, chrom < 23)
  mat = get.cumulative.chr.maploc(mat, load.genome)
  mid = mat$mid
  mat = mat$mat

  cncf = fit$cncf
  cncf = subset(cncf, chrom < 23)

  mafR = rep(sqrt(abs(cncf$mafR)), cncf$num.mark)
  mat = cbind(mat, mafR)

  starts = cumsum(c(1,cncf$num.mark))[1:length(cncf$num.mark)]
  ends = cumsum(c(cncf$num.mark))
  my.starts = mat[starts,c('chr.maploc','mafR')]
  my.ends = mat[ends,c('chr.maploc','mafR')]

  if(is.null(sample.num)){subset_ = 1:nrow(mat)}

  if(is.null(sample.num) == FALSE){
    if(sample.num >= nrow(mat)){subset_ = 1:nrow(mat)}
    if(sample.num < nrow(mat)){subset_ = sort(sample(1:nrow(mat), sample.num, replace=FALSE))}
  }

  if (!is.null(subset.indices)) {
    mat = mat[subset.indices,]
  } else { mat = mat[subset_,] }

  col.rep = 1 + rep(mat$chrom - 2 * floor(mat$chrom/2))
  pt.cols = c(col.1, col.2)[col.rep]

  valor = ggplot(mat, environment = environment())
  if(!is.null(gene.pos)){
    valor = valor + geom_vline(xintercept=gene.pos$mid, color='palevioletred1')
    mat$gene = FALSE
    mat$gene[which(mat$chrom == gene.pos$chrom & mat$maploc >= gene.pos$start & mat$maploc <= gene.pos$end)] = TRUE
  } else { mat$gene = FALSE }

  valor = valor +
    geom_point(aes(y=valor,x=chr.maploc), colour=pt.cols, size=.4) +
    geom_point(data = subset(mat, gene==T), aes(y=valor,x=chr.maploc), color='#525252', size=.4) +
    scale_x_continuous(breaks=mid, labels=names(mid)) +
    xlab('') +
    ylim(-4,4) +
    ylab('Log Odds Ratio') +
    geom_segment(data=cncf, aes(x=my.starts$chr.maploc, xend=my.ends$chr.maploc, yend=my.ends$mafR, y=my.starts$mafR), col='red3', size=1, lineend=lend) +
    geom_segment(data=cncf, aes(x=my.starts$chr.maploc, xend=my.ends$chr.maploc, yend=-my.ends$mafR, y=-my.starts$mafR), col='red3', size=1, lineend=lend)

  panel.grid.col='white'; grid.width = .5
  if(theme=='bw'){panel.grid.col='grey'; grid.width = .2; valor = valor + theme_bw()}

  valor = valor + theme(axis.text.x = element_text(angle=0, size=8),
                        axis.text.y = element_text(angle=0, size=8),
                        text = element_text(size=10),
                        panel.grid.minor.x=element_line(colour=panel.grid.col, size=grid.width),
                        panel.grid.major.x=element_line(colour=panel.grid.col, size=0),
                        plot.margin = unit(c(0,1,0,0), 'lines'))
  valor
}

#' helper function for app
#'
#' @param out
#' @return simple metadata data.frame
#' @import dplyr
#' @export cellular.fraction
cellular.fraction = function(out, fit, method=c('cncf', 'em'), load.genome=FALSE, gene.pos=NULL, main='', lend='butt', theme='bw', ...){

  mat = out$jointseg
  mat = subset(mat, chrom < 23)
  mat = get.cumulative.chr.maploc(mat, load.genome)
  mid = mat$mid
  mat = mat$mat

  cncf = fit$cncf
  cncf = subset(cncf, chrom < 23)

  if(method == 'em'){cncf$cf.em[is.na(cncf$cf.em)] = -1; cf = rep(cncf$cf.em, cncf$num.mark); my.ylab='EM - CF'}
  if(method == 'cncf'){cncf$cf[is.na(cncf$cf)] = -1; cf = rep(cncf$cf, cncf$num.mark); my.ylab='CNCF - CF'}

  mat = cbind(mat, cf)
  starts = cumsum(c(1,cncf$num.mark))[1:length(cncf$num.mark)]
  ends = cumsum(c(cncf$num.mark))
  my.starts = mat[starts,c('chr.maploc','cf')]
  my.ends = mat[ends,c('chr.maploc','cf')]

  cf = ggplot(mat, environment = environment())
  if(!is.null(gene.pos)){
    cf = cf + geom_vline(xintercept=gene.pos$mid, color='palevioletred1')
  }

  cf = cf +
    geom_segment(data=cncf, aes(x=my.starts$chr.maploc, xend=my.ends$chr.maploc, yend=my.ends$cf, y=my.starts$cf), col='black', size=1, lineend=lend) +
    scale_x_continuous(breaks=mid, labels=names(mid)) +
    xlab('') +
    ylim(0,1) +
    ylab(my.ylab)

  panel.grid.col='white'; grid.width = .5
  if(theme=='bw'){panel.grid.col='grey'; grid.width = .2; cf = cf + theme_bw()}

  cf = cf + theme(axis.text.x  = element_text(angle=90, vjust=0, size=8),
                  axis.text.y = element_text(angle=90, vjust=0, size=8),
                  text = element_text(size=10),
                  panel.grid.minor.x=element_line(colour=panel.grid.col, size=grid.width),
                  panel.grid.major.x=element_line(colour=panel.grid.col, size=0),
                  plot.margin = unit(c(0,1,0,0), 'lines'))
  cf
}

#' helper function for app
#'
#' @param out
#' @return simple metadata data.frame
#' @import dplyr
#' @export integer.copy.number
integer.copy.number = function(out, fit, method=c('cncf', 'em'), load.genome=FALSE, gene.pos=NULL, main='', lend='butt', theme='bw', ...){

  mat = out$jointseg
  mat = subset(mat, chrom < 23)
  mat = get.cumulative.chr.maploc(mat, load.genome)
  mid = mat$mid
  mat = mat$mat

  cncf = fit$cncf
  cncf = subset(cncf, chrom < 23)

  if(method == 'em'){tcnscaled = cncf$tcn.em; tcnscaled[cncf$tcn.em > 5 & !is.na(cncf$tcn.em)] = (5 + (tcnscaled[cncf$tcn.em > 5 & !is.na(cncf$tcn.em)] - 5)/3)}
  if(method == 'cncf'){tcnscaled = cncf$tcn; tcnscaled[cncf$tcn > 5 & !is.na(cncf$tcn)] = (5 + (tcnscaled[cncf$tcn > 5 & !is.na(cncf$tcn)] - 5)/3)}
  tcn_ = rep(tcnscaled, cncf$num.mark)

  if(method == 'em'){lcn_ = rep(cncf$lcn.em, cncf$num.mark); my.ylab='Integer CN (EM)'}
  if(method == 'cncf'){lcn_ = rep(cncf$lcn, cncf$num.mark); my.ylab='Integer CN (CNCF)'}

  mat = cbind(mat, cbind(tcn_, lcn_))
  starts = cumsum(c(1,cncf$num.mark))[1:length(cncf$num.mark)]
  ends = cumsum(c(cncf$num.mark))
  my.tcn.starts = mat[starts,c('chr.maploc','tcn_')]
  my.tcn.ends = mat[ends,c('chr.maploc','tcn_')]
  my.lcn.starts = mat[starts,c('chr.maploc','lcn_')]
  my.lcn.ends = mat[ends,c('chr.maploc','lcn_')]

  icn = ggplot(mat, environment = environment())
  if(!is.null(gene.pos)){
    icn = icn + geom_vline(xintercept=gene.pos$mid, color='palevioletred1')
  }

  icn = icn +
    geom_segment(data=cncf, aes(x=my.lcn.starts$chr.maploc, xend=my.lcn.ends$chr.maploc, y=my.lcn.starts$lcn_, yend=my.lcn.ends$lcn_), col='red', size=1, lineend=lend) +
    geom_segment(data=cncf, aes(x=my.tcn.starts$chr.maploc, xend=my.tcn.ends$chr.maploc, y=my.tcn.starts$tcn_, yend=my.tcn.ends$tcn_), col='black', size=1,lineend=lend) +
    scale_y_continuous(breaks=c(0:5, 5 + (1:35)/3), labels=0:40,limits = c(0, NA)) +
    scale_x_continuous(breaks=mid, labels=names(mid)) +
    ylab(my.ylab) +
    xlab('')

  panel.grid.col='white'; grid.width = .5
  if(theme=='bw'){panel.grid.col='grey'; grid.width = .2; icn = icn + theme_bw()}

  icn = icn + theme(axis.text.x  = element_text(angle=0, size=8),
                    axis.text.y = element_text(angle=0, size=8),
                    text = element_text(size=10),
                    panel.grid.minor.x=element_line(colour=panel.grid.col, size=grid.width),
                    panel.grid.major.x=element_line(colour=panel.grid.col, size=0),
                    plot.margin = unit(c(0,1,0,0), 'lines'))

  icn
}

#' helper function for app
#'
#' @param out
#' @return simple metadata data.frame
#' @import dplyr
#' @export get.cumulative.chr.maploc
get.cumulative.chr.maploc = function(mat, load.genome=FALSE){

  chrom.lengths = c(249250621, 243199373, 198022430, 191154276, 180915260, 171115067, 159138663, 146364022, 141213431, 135534747,135006516, 133851895, 115169878, 107349540, 102531392, 90354753,  81195210,  78077248,  59128983,  63025520, 48129895,  51304566) #hg19

  cum.chrom.lengths = cumsum(as.numeric(chrom.lengths))
  mid = cum.chrom.lengths - (chrom.lengths/2)
  names(mid) = 1:22

  chr.maploc.to.gen.maploc = function(x){mat[mat$chrom==x,]$maploc + cum.chrom.lengths[x-1]}
  chr.maploc = sapply(2:22,chr.maploc.to.gen.maploc)
  chr.maploc = unlist(chr.maploc)
  chr.maploc = c(mat[mat$chrom==1,]$maploc,chr.maploc)
  mat = cbind(mat,chr.maploc)

  list(mat=mat, mid=mid)
}

#' helper function for app
#'
#' @return simple metadata data.frame
#' @import dplyr
#' @export get.gene.pos.cached
get.gene.pos.cached <- function(hugo.symbol, my.path='/ifs/work/bandlamc/git/facets-suite/Homo_sapiens.GRCh37.75.gene_positions.txt') {
  fread(my.path) %>%
    filter(gene == hugo.symbol)
}

#' helper function for app
#'
#' @param out
#' @return simple metadata data.frame
#' @import dplyr
#' @export close.up
close.up = function(out, fit, chrom.range=NULL, method=NA, gene.name=NULL, lend='butt', bed.path=NULL, cached.gene.path = NULL, subset.snps=FALSE, ...){

  #if (!is.null(bed.path)) { gene.info = get.gene.pos(gene.name, my.path = bed.path)
  #} else { gene.info = get.gene.pos(gene.name) }

  if (!is.null(cached.gene.path)) {gene.info = get.gene.pos.cached(gene.name, my.path = cached.gene.path)
  } else if (!is.null(bed.path)) { gene.info = get.gene.pos(gene.name, my.path = bed.path)
  } else { gene.info = get.gene.pos(gene.name) }

  if (!is.null(gene.name)) gene.pos = gene.info
  if (is.null(gene.name)) gene.pos = NULL
  if (is.null(chrom.range)) chrom.range = min(gene.info$chrom):max(gene.info$chrom)

  out$out = out$out[out$out$chrom %in% chrom.range,]
  out$jointseg = out$jointseg[out$jointseg$chrom %in% chrom.range,]
  out$IGV = out$IGV[out$IGV$chrom %in% chrom.range,]
  fit$cncf = fit$cncf[fit$cncf$chrom %in% chrom.range,]

  subset.indices = NULL

  cnlr = copy.number.log.ratio(out, fit, gene.pos=gene.pos, lend=lend, subset.indices=subset.indices, ...)
  valor = var.allele.log.odds.ratio(out, fit, gene.pos=gene.pos, lend=lend, subset.indices=subset.indices, ...)

  output_list <- list(cnlr=cnlr,valor=valor)
  if(method == 'em' | is.na(method)){
    cfem = cellular.fraction(out, fit, method='em', gene.pos=gene.pos, lend=lend, ...)
    icnem = integer.copy.number(out, fit, method='em', gene.pos=gene.pos, lend=lend, ...)
    output_list <- c(output_list, list(cfem=cfem, icnem=icnem))
  }
  if(method == 'cncf' | is.na(method)){
    cfcncf = cellular.fraction(out, fit, method='cncf', gene.pos=gene.pos, lend=lend, ...)
    icncncf = integer.copy.number(out, fit, method='cncf', gene.pos=gene.pos, lend=lend, ...)
    output_list <- c(output_list, list(cfcncf=cfcncf, icncncf=icncncf))
  }
  c(output_list, list(chrom=gene.info$chrom, start=gene.info$start, end=gene.info$end))
}
