
split_nchars_numeric<-function(string,no_chars){
    # FOR READING STRUCTURED TEXT
    # Take a string, split it into individual characters, and recombine them
    # into separate pieces of no_chars length. Then convert these to numeric,
    # and output as a vector

    tmp=unlist(strsplit(string, split=""))
    l=length(tmp)
    if(l%%no_chars!=0){
        print(c('tmp = ', tmp))
        print(c('no_chars = ', no_chars))
        print(c('Length tmp = ', l))
        stop('ERROR in split_nchars_numeric: Number of characters in string is not a multiple of no_chars')

    }

    l2=l/no_chars
    output=rep(NA,l2)

    for(i in 1:l2){
        output[i] = as.numeric(paste(tmp[(1:no_chars) + no_chars*(i-1)], sep="", collapse=""))
    }
    output
}



replace_with_lidar<-function(reach_name, sect_name, section){
    # Take the cross-section in 'reach_name' named 'sect_name' with data 'section' from hec-ras
    # Compute the spatial locations of section, and extract the LIDAR data in the same place.
    # Then output the section data with the lidar used in place of the original data where appropriate.

    section2=section
    section2[,2]=section[,2]+2*(runif(length(section[,2]))-0.5)

    section2
}
