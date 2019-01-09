library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(tidyr)
specify_decimal = function(x, k) format(round(x, k), nsmall=k)
"%ni%" = Negate("%in%")

###Edit this bloack for appropriate inputs

facets_dir = '/ifs/res/taylorlab/chavans/roslin_2.4_deliveries/all_facets'


facets_cm_files=list.files(facets_dir,pattern="*.dat.gz$",full.names=TRUE)
facets_txt_files=list.files(facets_dir,pattern="*.txt",full.names=TRUE)
facets_png_files=list.files(facets_dir,pattern="*.png",full.names=TRUE)
facets_out_files=list.files(facets_dir,pattern="*.out",full.names=TRUE)
facets_seg_files=list.files(facets_dir,pattern="*.seg",full.names=TRUE)
facets_Rdata_files=list.files(facets_dir,pattern="*.Rdata",full.names=TRUE)

output_dir = '/ifs/res/taylorlab/chavans/roslin_2.4_deliveries/all_facets/'
sample_pairing = fread('/ifs/res/taylorlab/chavans/roslin_2.4_deliveries/all_sample_pairing/master_sample_pairing_123118.txt')
names(sample_pairing)=c("N","T")
sample_pairing<-filter(sample_pairing,N!="na" & T!="na")
total_tumors=dim(sample_pairing)[1]
print(paste0(total_tumors," samples"))
#setwd(output_dir)

###########################################

for(i in 1:total_tumors)
{
  sample_dir_name = paste0(output_dir,sample_pairing$T[i],'__',sample_pairing$N[i])
  if(!dir.exists(sample_dir_name))
  {
    facets_sample_dir_name = paste0(sample_dir_name,"/facets_R0.5.6c100p500")
    cmd1 = paste0("mkdir -p ",facets_sample_dir_name)
    system(cmd1)
   
    print(sample_pairing$T[i])
    cm_file = gsub(",", "",toString(facets_cm_files[as.numeric(grep(sample_pairing$T[i],facets_cm_files))]))
    print(cm_file)
  
    cmd_cm_rename = paste0("countsMerged____",sample_pairing$T[i],"__",sample_pairing$N[i],".dat.gz")
    cmd2 = paste0("mv ",cm_file," ",sample_dir_name,"/",cmd_cm_rename)
    print(cmd2)
    system(cmd2)
    
    #print(cmd_cm_rename)
    
    txt_file = gsub(",", "",toString(facets_txt_files[as.numeric(grep(sample_pairing$T[i],facets_txt_files))]))
    cmd3 = paste0("mv ",txt_file," ",facets_sample_dir_name)
    system(cmd3)
    
    png_file = gsub(",", "",toString(facets_png_files[as.numeric(grep(sample_pairing$T[i],facets_png_files))]))
    cmd4 = paste0("mv ",png_file," ",facets_sample_dir_name)
    system(cmd4)
    
    seg_file = gsub(",", "",toString(facets_seg_files[as.numeric(grep(sample_pairing$T[i],facets_seg_files))]))
    cmd5 = paste0("mv ",seg_file," ",facets_sample_dir_name)
    system(cmd5)
    
    out_file = gsub(",", "",toString(facets_out_files[as.numeric(grep(sample_pairing$T[i],facets_out_files))]))
    cmd6 = paste0("mv ",out_file," ",facets_sample_dir_name)
    system(cmd6)
    
    Rdata_file = gsub(",", "",toString(facets_Rdata_files[as.numeric(grep(sample_pairing$T[i],facets_Rdata_files))]))
    cmd7 = paste0("mv ",Rdata_file," ",facets_sample_dir_name)
    system(cmd7)
    }
}

print(paste0("Done :).."))
