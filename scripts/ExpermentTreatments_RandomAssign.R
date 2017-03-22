# Randomizations For IL Tollway Planting Experiments
# Code: C. Rollinson
# 
# Contents:
# Experiment 1: Soil Ammendments (M. Midgley, lead)
# Experiment 2: Planting size (G. Watson, lead)

save.out <- "~/Google Drive/IL Tollway/"


# ------------------------------------
# Experiment 1: Soil Ammendments
# Design Basics: Random Block Design
# -- Soil treatments:
#    1. Control (no ammendment)
#    2. Biosolids
#    3. Composted biosolids
#    4. Leaf Compost
#    5. 50:50 Biosolid-Biochar mix
# -- Species: total 6
#    -- hardiness: tough, intermediate, sensitive
#    -- Mycorrhizal Association: Arbuscular mycorrhizal (AM), ectomycorrhizal (ECM)
# 2 Parts: Dense & Scattered Tree Plantings
# -- Part A: Dense Tree: Random Block Design w/ treatments in plots
#    -- 4 blocks; 5 treatments (plots) per block, 15 trees per plot (3 rows of 5)
# -- Part B: Scattered Tree: Random Block Design with treatments on trees
# ------------------------------------
species <- data.frame(common.name.     =c("northern catalpa", "honeylocust"          , "tulip tree"             , "shagbark hickory", "sugar maple"   , "red oak"      ),
                      scientific.name  =c("Catalpa speciosa", "Gleditsia triacanthos", "Liriodendron tulipifera", "Carya ovata"     , "Acer saccharum", "Quercus rubra"),
                      mycorrhizal.assoc=c("arbuscular"      , "ecto"                 , "arbuscular"             , "ecto"            , "arbuscular"    , "ecto"         ),
                      toughness.       =c("tough"           , "tough"                , "intermediate"           , "intermediate"    , "sensitive"     , "sensitive"    ))


# -------------------
# Part A: Dense Tree Planting
# -------------------
blocks <- 1:4
plots <- 1:20
trees <- 1:15
treatments <- c("Control", "Biosolids", "Composted Biosolids", "Leaf Compost", "Biosolid-Biochar Mix")

# Setting up the plot treatment data frame
plot.df <- data.frame(block = as.factor(rep(blocks, each=length(treatments))), plot=as.factor(plots))

set.seed(1529) # Setting the seed so this can be reproduced
for(i in blocks){
	treats <- sample(treatments, 5, replace=F)
	plot.df[plot.df$block==i, "treatment"] <- treats
}
plot.df$treatment <- as.factor(plot.df$treatment)

tree.df <- merge(plot.df, data.frame(tree=as.factor(trees), species=NA))
dim(tree.df)
summary(tree.df)

# Sorting by block-plot
tree.df <- tree.df[order(tree.df$block, tree.df$plot, tree.df$tree),]
tree.df[1:25,]

# Addding in species associations
# Note: Trees will not be evenly represented within plots: some will have 2, some will have 3
#       However, all trees should be evenly represented across treatments
#       10 trees per treatment spread over 4 blocks -- 2 blocks have 2 trees, 2 blocks have 3 trees per treatment
#
#       We need a mechanism that ensures that each plot gets at least 2 of each species
#       Best Solution I could think of: 
#        Step 1: Randomly Assign 2 individuals for each species
#        Step 2: Going by treatment, randomly assign 1 indiviual to 2 more plots

# Step 1: Randomly assigning 2 individuals for each species
set.seed(1357)
for(i in unique(species$common.name)){
	for(p in unique(tree.df$plot)){
		# Randomly pick 2 unassigned trees
		trees.now <- sample(unique(tree.df[tree.df$plot==p & is.na(tree.df$species), "tree"]), 2, replace=F)
		
		# Assigning a species to trees
		tree.df[tree.df$plot==p & tree.df$tree %in% trees.now, "species"] <- paste(i)
	}
}
tree.df[1:30,]

# Step 3: Dealing with the leftovers
set.seed(414)
for(j in unique(tree.df$treatment)){
	for(i in unique(species$common.name)){
		# Find which blocks have room for an extra tree
		block.extra <- sample(unique(tree.df[tree.df$treatment==j & is.na(tree.df$species), "block"]), 2, replace=F)
		
		for(b in block.extra){
			# Randomly pick 1 of the available trees
			trees.now <- sample(unique(tree.df[tree.df$treatment==j & tree.df$block==b & is.na(tree.df$species), "tree"]), 1, replace=F)
			
			tree.df[tree.df$treatment==j & tree.df$block==b & tree.df$tree %in% trees.now, "species"] <- paste(i)
		} # End tree asignment (block.extra)
	} # End Species
} # End Treatment
tree.df$species <- as.factor(tree.df$species)
summary(tree.df)
tree.df[1:30,]

summary(tree.df[tree.df$treatment=="Control",])
summary(tree.df[tree.df$treatment=="Biosolids",])
summary(tree.df[tree.df$treatment=="Composted Biosolids",])
summary(tree.df[tree.df$treatment=="Leaf Compost",])
summary(tree.df[tree.df$treatment=="Biosolid-Biochar Mix",])

write.csv(plot.df, file.path(save.out, "SoilAmmendments_DenseTree_TreatmentAssignments_Plots.csv"), row.names=F)
write.csv(tree.df, file.path(save.out, "SoilAmmendments_DenseTree_TreatmentAssignments_Trees.csv"), row.names=F)
# -------------------


# -------------------
# Part B: Scattered Tree
# Randomly Assign Trees + Treatments to each block
# All Species as Part A, but subset of treatments
# -- 1 tree of each species/treatment per block
# -------------------
treatments <- c("Control", "Biosolids", "Composted Biosolids")
blocks <- 1:9
trees <- 1:18

# Creating the data frame for assignment 
tree.df <- data.frame(block = as.factor(rep(blocks, each=length(trees))), tree=as.factor(trees), treatment=NA, species=NA)
summary(tree.df)
dim(tree.df)
tree.df[1:25,]

# Making a loop to randomly assign species & treatments
set.seed(851)
for(i in unique(tree.df$block)){
  for(spp in unique(species$common.name)){
    # Randomly pick 3 unassigned trees
    trees.now <- sample(unique(tree.df[tree.df$block==i & is.na(tree.df$species), "tree"]), 3, replace=F)

    tree.df[tree.df$block==i & tree.df$tree %in% trees.now, "species"] <- spp  

    # Randomly assign an order to the treatmetns for those three trees
    # Note: Using a loop otherwise trees will get assigned in numerical order, 
    #       which we don't necessarily want
    treats.now <- sample(treatments, 3, replace=F)
    for(j in 1:length(trees.now)){
      tree.df[tree.df$block==i & tree.df$tree==trees.now[j], "treatment"] <- treats.now[j]
    }
  } # end species assignment
} # End treatment assignment

tree.df$treatment <- as.factor(tree.df$treatment)
tree.df$species <- as.factor(tree.df$species)
summary(tree.df)
tree.df[1:25,]

write.csv(tree.df, file.path(save.out, "SoilAmmendments_ScatteredTree_TreatmentAssignments_Trees.csv"), row.names=F)
# -------------------



# ------------------------------------
