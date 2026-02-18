##########################################################################################################
#
# pwrRasch: Statistical Power Simulation for Testing the Rasch Model
#
# Internal function: Computation of Three-Way ANOVA for unbalanced design
#
# Authors: Takuya Yanagida <takuya.yanagida@univie.ac.at>
#  		     Jan Steinfeld <jan.steinfeld@univie.ac.at>
#
##########################################################################################################

aov.rasch.sim.design <- function(data, group = "group", person = "person", item = "item", response = "response") {
  
  eval(parse(text = paste0("data$", group, " <- as.factor(data$", group, ")")))
  eval(parse(text = paste0("data$", person, " <- as.factor(data$", person, ")")))
  eval(parse(text = paste0("data$", item, " <- as.factor(data$", item, ")")))
  
  formula <- paste(response, "~", paste(group, item, paste(group, item, sep = ":"),
                                        paste0("Error(", person, " + ", person, ":", item,")"),sep = " + "))
 
  options(warn = -1)
     
  p.AC <- unlist(summary(aov(eval(parse(text = formula)), data = data)))["Error: person:item.Pr(>F)2"]
  names(p.AC) <- NULL

  options(warn = 0)
  
  return(p.AC)
  
}