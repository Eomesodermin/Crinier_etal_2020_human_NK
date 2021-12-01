

########################################
# Convert Gene names across species 
########################################

# Written by:
# October 14, 2016 by radiaj in R bloggers
# https://www.r-bloggers.com/2016/10/converting-mouse-to-human-gene-names-with-biomart-package/

# Usage
#genes <- convertMouseGeneList(humGenes)

# Convert mouse to human gene names
convertMouseGeneList <- function(x){
  
  require("biomaRt")
  human = useMart("ensembl", 
                  dataset = "hsapiens_gene_ensembl")
  
  mouse = useMart("ensembl", 
                  dataset = "mmusculus_gene_ensembl")
  
  genesV2 = getLDS(attributes = c("mgi_symbol"),
                   filters = "mgi_symbol",
                   values = x , 
                   mart = mouse,
                   attributesL = c("hgnc_symbol"),
                   martL = human, 
                   uniqueRows=T)
  
  humanx <- unique(genesV2[, 2])
  
  # Print the first 6 genes found to the screen
  print(head(humanx))
  return(humanx)
}


# Convert human to mouse gene names
convertHumanGeneList <- function(x){
  
  require("biomaRt")
  human = useMart("ensembl", 
                  dataset = "hsapiens_gene_ensembl")
  
  mouse = useMart("ensembl", 
                  dataset = "mmusculus_gene_ensembl")
  
  genesV2 = getLDS(attributes = c("hgnc_symbol"),
                   filters = "hgnc_symbol",
                   values = x , 
                   mart = human, 
                   attributesL = c("mgi_symbol"),
                   martL = mouse, 
                   uniqueRows=T)
  
  humanx <- unique(genesV2[, 2])
  
  # Print the first 6 genes found to the screen
  print(head(humanx))
  return(humanx)
}

############################################################
# Make a transparent version of any given colour
############################################################

# Written by Ricardo Oliveros-Ramos
# https://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color
# Usage
# makeTransparent("red", "blue", alpha=0.8)

makeTransparent = function(..., alpha = 0.5) {
  
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  
  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  
  return(newColor)
  
}




