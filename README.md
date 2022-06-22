# Plot_label_shiny
  
Designing a Shiny for lab use, which assists in the creation of a scatterplot, then allows the user to add labels to specific data points  
  
### Wishlist
* Currently fixing gene list download to be a data frame of name/x/y/group  
* Make ui more user friendly  
* What if a gene gets put in more than one input list?  
* Make it clear in the interface when a log transformation has been applied to the returned y-values, or convert back to original for display  
* What to do about unlabeled data points when too many overlaps?  
* Possibly update download options for gene lists to order by name or by group  
* Within server, some of the variable names are from the first iteration, like LFC, and should be updated to better reflect current funcitonality of the app  
* Verify that everywhere I do an index by gene name (thinking specifically of getting gene names the range values for x & y), check that it works with Rownames for gene id as well as colname for gene id.   
* Currently input for y-label is being used for both the plot, and the output of downloaded genes. This should be updated because it might be useful to indicated that the plot is log10 scaled, but to download the original data values without the transformation  
  
  