# The samsungData seems to have an error in column labeling by having
# forgotten the X, Y, and Z extension in the "bandsEnergy()" columns,
# and has three sets of duplicate names (9 blocks of such names). Fix
# this by putting the X, Y, and Z into the names. Return a vector
# whose field names are the new names, and the values are the old
# names. In other words, the returned vector can be viewed as a
# mapping from its row names (which are the new variable names
# proposed for the data frame) to its values (which are the existing
# variable names in the dataframe). Be verbose by default. Also make
# the variables into legal R variable names by default. The dataframe
# is not actually changed. If desired, it can be updated with
# names(samsungData) <- names(fixed.name.mapping(samsungData)).

fixed.name.mapping.samsungData <-  function(df, legal=TRUE, verbose=FALSE){
  axis <- c("X","Y","Z")
  index.axis <- 0
  oldnames = names(df)
  name.mapping=character()
  for(i in seq_along(oldnames)){
    this.oldname <- oldnames[i]
    name.mapping[i] <- this.oldname
    if(regexpr("-bandsEnergy\\(\\)-1,8$", this.oldname) > 0){
      # start a new block of repeated columns
      index.axis <- index.axis + 1
      if(index.axis > 3) index.axis <- index.axis - 3
      if(verbose) print(paste("new block, setting axis to ",
                              axis[index.axis]), sep="")
    }
    if(regexpr("-bandsEnergy\\(\\)-", this.oldname) > 0){
      # update the name by inserting the axis
      this.newname =
        sub(pattern="-bandsEnergy\\(\\)-",
            replacement=
              paste("-bandsEnergy\\(\\)-", axis[index.axis], ",", sep=""),
            x=this.oldname)
      if(verbose){
        print(paste("oldnames[",i,"]=",this.oldname,sep=""))
        print(paste("newnames[",i,"]=",this.newname,sep=""))
      }
    } else {
      this.newname <- this.oldname
    }
    if(legal){
      # also make it a legal R variable name
      this.newname =
        gsub(pattern="[^a-zA-Z_0-9.]", replacement=".", x=this.newname)
      # might as well make it pretty
      this.newname =
        gsub(pattern="\\.{2,}", replacement=".", x=this.newname)
      this.newname =
        gsub(pattern="\\.$", replacement="", x=this.newname)
    }
    names(name.mapping)[i] <- this.newname
  }
  name.mapping
}