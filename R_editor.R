### R code to edit the .g01 files of HEC_RAS

### Idea is to read in the file (which is a special structured text format)
### and replace the cross-section values with the values we want

input_file='./original_hecras/Backup.g01' # HEC_RAS geometry file
output_file='with_lidar.g03'
output_dir='./modified_hecras/'

source('useful_functions.R') # Lidar + string manipulation

### MAIN CODE


fin=file(input_file,open='r') # Open file in read-only mode

file_contents=readLines(fin) # Read the contents
output_contents=file_contents # Edit this as we go

# Search for the '#' character, which can be used to find cross-sections
# Use this to define the lines which hold the cross-sectional data
tmp=grep('#', file_contents) # In between each pair is the cross-sectional data
xsect_bounds=matrix(tmp,ncol=2,byrow=T) # These pairs can be used to get the cross-sectional data
xsect_bounds[,1]=xsect_bounds[,1]+1
xsect_bounds[,2]=xsect_bounds[,2]-1


# Search for the river reach names
reach_indices = grep('River Reach', file_contents)
reach_names = file_contents[reach_indices]

# Find the names of the cross-sections, which practically correspond with their upstream distance
section_indices=grep('Type RM', file_contents) # Line numbers which hold the cross-section name
section_head=file_contents[section_indices]    # The line
section_names=section_head         # This will hold the name
section_reach_names=section_head   # This will hold the reach name of the section 
tmp=strsplit(section_head,",")
for(i in 1:length(section_names)){
    section_names[i]=as.numeric(tmp[[i]][2])
    reach_index = sum(section_indices[i]>reach_indices) 
    section_reach_names[i] = reach_names[reach_index]

}

# Error check
if(length(section_names)!=length(xsect_bounds[,1])){
    stop('ERROR: There are a different number of cross-sectional names and cross-sections')
}


for(i in 1:length(xsect_bounds[,1])){
    #print(i)
    # Get the x-z section data as a character, and coerce it to a matrix of x,z
    xz_section_data=file_contents[xsect_bounds[i,1]:xsect_bounds[i,2]]

    # Force the data into a numeric form
    tmp1=unlist(strsplit(xz_section_data, " "))
   
    # Split this up into a numeric vector  
    tmp1=split_nchars_numeric(xz_section_data, 8)
    
    section=matrix(tmp1, ncol=2,byrow=T)
   
    # Get the cross-sectional data names
    sect_name=section_names[i]    
    sect_reach_name = section_reach_names[i]
    #write.table(section, file=paste(output_dir, sect_name, '.txt', sep=""), row.names=F, col.names=F)
    #stop()

    # Hypothetical editing of the section elevation values
    # Here, we can instead do the lidar interpolation
    section2=replace_with_lidar(sect_reach_name, sect_name, section)
    #section2[,2]=section[,2]+2.0*(runif(length(section[,2]))-0.5)

    # Now we need to write the output

    # Convert back to a character string
    # We need to hack to get the order right
    tmp6=c(section2)
    for(j in 1:length(section2[,1])){
        indz=2*j-1
        tmp6[indz:(indz+1)]=section2[j,1:2]
    }
    # Convert to character, with 7 characters in each
    tmp7= format(as.character(signif(tmp6,5)),trim=T,width=8,justify='right')
    
    # Write the result back to a character vector of the same form as xz_section_data
    tmp_out=xz_section_data
    if(length(tmp_out)>1){
        for(j in 1:(length(tmp_out)-1)){
            tmp_out[j]=paste(tmp7[1:10+(j-1)*10], collapse="")
        }
    }
    tmp_out[length(tmp_out)]=paste(tmp7[ ((length(tmp_out)-1)*10+1):length(tmp7)], collapse="")
    output_contents[xsect_bounds[i,1]:xsect_bounds[i,2]]=tmp_out
}

close(fin)

cat(output_contents,file=paste(output_dir,output_file,sep=""),sep="\n")
